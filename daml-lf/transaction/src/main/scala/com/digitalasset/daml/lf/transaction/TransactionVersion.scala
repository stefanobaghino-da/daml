// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package transaction

import com.daml.lf.data.Ref.PackageId
import com.daml.lf.data.{ImmArray, Ref}
import com.daml.lf.language.LanguageVersion
import com.daml.lf.value.Value.{ContractId, VersionedValue}
import com.daml.lf.value.{Value, ValueVersion, ValueVersions}

import scala.collection.immutable.HashMap

final case class TransactionVersion(protoValue: String)

/**
  * Currently supported versions of the DAML-LF transaction specification.
  */
private[lf] object TransactionVersions
    extends LfVersions(versionsAscending = VersionTimeline.ascendingVersions[TransactionVersion])(
      _.protoValue,
    ) {

  import VersionTimeline._
  import VersionTimeline.Implicits._

  private[this] val minVersion = TransactionVersion("1")
  private[transaction] val minKeyOrLookupByKey = TransactionVersion("3")
  private[transaction] val minFetchActors = TransactionVersion("5")
  private[transaction] val minNoControllers = TransactionVersion("6")
  private[transaction] val minExerciseResult = TransactionVersion("7")
  private[transaction] val minContractKeyInExercise = TransactionVersion("8")
  private[transaction] val minMaintainersInExercise = TransactionVersion("9")
  private[transaction] val minContractKeyInFetch = TransactionVersion("10")
  private[transaction] val minChoiceObservers = TransactionVersion("dev")

  // Older versions are deprecated https://github.com/digital-asset/daml/issues/5220
  val StableOutputVersions: VersionRange[TransactionVersion] =
    VersionRange(TransactionVersion("10"), TransactionVersion("10"))

  val DevOutputVersions: VersionRange[TransactionVersion] =
    StableOutputVersions.copy(max = acceptedVersions.last)

  val Empty: VersionRange[TransactionVersion] =
    VersionRange(acceptedVersions.last, acceptedVersions.head)

  def assignVersion(
      a: GenTransaction.WithTxValue[_, Value.ContractId],
      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
  ): Either[String, TransactionVersion] = {
    require(a != null)
    import VersionTimeline.Implicits._

    val inferredVersion =
      VersionTimeline.latestWhenAllPresent(
        supportedVersions.min,
        // latest version used by any value
        a.foldValues(ValueVersion("1")) { (z, vv) =>
          VersionTimeline.maxVersion(z, vv.version)
        },
        // a NodeCreate with defined `key` or a NodeLookupByKey
        // implies minimum version 3
        if (a.nodes.values.exists {
            case nc: Node.NodeCreate[_, _] => nc.key.isDefined
            case _: Node.NodeLookupByKey[_, _] => true
            case _: Node.NodeFetch[_, _] | _: Node.NodeExercises[_, _, _] => false
          }) minKeyOrLookupByKey
        else
          minVersion,
        // a NodeFetch with actingParties implies minimum version 5
        if (a.nodes.values
            .exists { case nf: Node.NodeFetch[_, _] => nf.actingParties.nonEmpty; case _ => false })
          minFetchActors
        else
          minVersion,
        if (a.nodes.values
            .exists {
              case ne: Node.NodeExercises[_, _, _] => ne.exerciseResult.isDefined
              case _ => false
            })
          minExerciseResult
        else
          minVersion,
        if (a.nodes.values
            .exists {
              case ne: Node.NodeExercises[_, _, _] => ne.key.isDefined
              case _ => false
            })
          minContractKeyInExercise
        else
          minVersion,
        if (a.nodes.values
            .exists {
              case ne: Node.NodeExercises[_, _, _] =>
                ne.key match {
                  case Some(Node.KeyWithMaintainers(key @ _, maintainers)) => maintainers.nonEmpty
                  case _ => false
                }
              case _ => false
            })
          minMaintainersInExercise
        else
          minVersion,
        if (a.nodes.values
            .exists {
              case nf: Node.NodeFetch[_, _] => nf.key.isDefined
              case _ => false
            })
          minContractKeyInFetch
        else
          minVersion,
      )

    Either.cond(
      !(supportedVersions.max precedes inferredVersion),
      inferredVersion,
      s"inferred version $inferredVersion is not supported"
    )

  }

  // FIXME: use or drop
//  def asVersionedTransaction(
//      tx: GenTransaction.WithTxValue[NodeId, Value.ContractId],
//      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
//  ): Either[String, Transaction.Transaction] =
//    for {
//      v <- assignVersion(tx, supportedVersions)
//    } yield VersionedTransaction(v, tx)
//
//  @throws[IllegalArgumentException]
//  def assertAsVersionedTransaction(
//      tx: GenTransaction.WithTxValue[NodeId, Value.ContractId],
//      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
//  ): Transaction.Transaction =
//    data.assertRight(asVersionedTransaction(tx, supportedVersions))

  private[lf] def assignValueVersion(transactionVersion: TransactionVersion): ValueVersion =
    latestWhenAllPresent(
      ValueVersions.acceptedVersions.head,
      transactionVersion,
    )

  private[lf] def assignVersions(
      supportedTxVersions: VersionRange[TransactionVersion],
      as: Seq[SpecifiedVersion],
  ): Either[String, TransactionVersion] = {

    val transactionVersion =
      VersionTimeline.latestWhenAllPresent(
        supportedTxVersions.min,
        (DevOutputVersions.min: SpecifiedVersion) +: as: _*,
      )

    Either.cond(
      !(supportedTxVersions.max precedes transactionVersion),
      transactionVersion,
      s"inferred transaction version ${transactionVersion.protoValue} is not allowed"
    )
  }

  def asVersionedNode(
      supportedTxVersions: VersionRange[TransactionVersion],
      pkgLangVersions: PackageId => language.LanguageVersion,
      node: Node.GenNode.WithValue[NodeId, ContractId],
  ): Either[String, Node.VersionedNode[NodeId, ContractId]] = {

    import VersionTimeline.Implicits._

    val langVersion: VersionTimeline.SpecifiedVersion = pkgLangVersions(node.templateId.packageId)

    TransactionVersions.assignVersions(supportedTxVersions, Seq(langVersion)).map { txVersion =>
      Node.VersionedNode(
        txVersion,
        Node.GenNode.map3(
          identity[NodeId],
          identity[ContractId],
          (v: Value[ContractId]) =>
            VersionedValue(TransactionVersions.assignValueVersion(txVersion), v)
        )(node)
      )
    }
  }

  def asVersionedTransaction(
      supportedTxVersions: VersionRange[TransactionVersion],
      pkgLangVersions: Ref.PackageId => LanguageVersion,
      roots: ImmArray[NodeId],
      nodes: HashMap[NodeId, Node.GenNode.WithValue[NodeId, ContractId]],
  ): Either[String, VersionedTransaction[NodeId, Value.ContractId]] = {

    def loop(
        nodes: Stream[(NodeId, Node.GenNode.WithValue[NodeId, ContractId])],
        acc: HashMap[NodeId, Node.VersionedNode[NodeId, ContractId]])
      : Either[String, VersionedTransaction[NodeId, Value.ContractId]] =
      nodes match {
        case (nodeId, node) #:: rest =>
          asVersionedNode(supportedTxVersions, pkgLangVersions, node) match {
            case Right(versionedNode) => loop(rest, acc.updated(nodeId, versionedNode))
            case Left(err) => Left(err)
          }
        case Stream.Empty =>
          Right(
            VersionedTransaction(
              acc.values.foldLeft(supportedTxVersions.min)((acc, node) =>
                VersionTimeline.maxVersion(acc, node.version)),
              acc,
              roots,
            ))
      }

    loop(nodes.toStream, HashMap.empty)
  }

}
