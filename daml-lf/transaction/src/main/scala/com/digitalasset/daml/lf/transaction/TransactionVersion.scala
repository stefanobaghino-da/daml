// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package transaction

import com.daml.lf.data.{ImmArray, Ref}
import com.daml.lf.language.LanguageVersion
import com.daml.lf.value.Value.VersionedValue
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

  // Older versions are deprecated https://github.com/digital-asset/daml/issues/5220
  val StableOutputVersions: VersionRange[TransactionVersion] =
    VersionRange(TransactionVersion("10"), TransactionVersion("10"))

  val DevOutputVersions: VersionRange[TransactionVersion] =
    StableOutputVersions.copy(max = acceptedVersions.last)

  val Empty: VersionRange[TransactionVersion] =
    VersionRange(acceptedVersions.last, acceptedVersions.head)

  private[transaction] def assignVersion(
      a: GenTransaction.WithTxValue[_, Value.ContractId],
      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
  ): Either[String, TransactionVersion] = {
    require(a != null)
    import VersionTimeline.Implicits._

    val inferredVersion =
      VersionTimeline.latestWhenAllPresent(
        supportedVersions.min,
        // latest version used by any value
        a.foldValues(ValueVersion("6")) { (z, vv) =>
          VersionTimeline.maxVersion(z, vv.version)
        }
      )

    Either.cond(
      !(supportedVersions.max precedes inferredVersion),
      inferredVersion,
      s"inferred version $inferredVersion is not supported"
    )
  }

  def asVersionedTransaction(
      tx: GenTransaction.WithTxValue[NodeId, Value.ContractId],
      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
  ): Either[String, Transaction.Transaction] =
    for {
      v <- assignVersion(tx, supportedVersions)
    } yield VersionedTransaction(v, tx)

  @throws[IllegalArgumentException]
  def assertAsVersionedTransaction(
      tx: GenTransaction.WithTxValue[NodeId, Value.ContractId],
      supportedVersions: VersionRange[TransactionVersion] = DevOutputVersions,
  ): Transaction.Transaction =
    data.assertRight(asVersionedTransaction(tx, supportedVersions))

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

  type UnversionedNode = Node.GenNode[NodeId, Value.ContractId, Value[Value.ContractId]]
  type VersionedNode = Node.GenNode[NodeId, Value.ContractId, VersionedValue[Value.ContractId]]

  def asVersionedTransaction(
      supportedTxVersions: VersionRange[TransactionVersion],
      pkgLangVersions: Ref.PackageId => LanguageVersion,
      roots: ImmArray[NodeId],
      nodes: HashMap[NodeId, UnversionedNode],
  ): Either[String, VersionedTransaction[NodeId, Value.ContractId]] = {

    import VersionTimeline.Implicits._

    val langVersions: Iterator[SpecifiedVersion] =
      roots.reverseIterator.map(nid => pkgLangVersions(nodes(nid).templateId.packageId))

    assignVersions(supportedTxVersions, langVersions.toList).map { txVersion =>
      val versionNode: UnversionedNode => VersionedNode =
        Node.GenNode.map3(identity, identity, VersionedValue(assignValueVersion(txVersion), _))
      VersionedTransaction(
        txVersion,
        GenTransaction(nodes = nodes.transform((_, n) => versionNode(n)), roots = roots)
      )
    }
  }

}
