// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.script.dump

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId, ZonedDateTime}

import com.daml.ledger.api.v1.transaction.{TransactionTree, TreeEvent}
import com.daml.ledger.api.v1.value.Value.Sum
import com.daml.ledger.api.v1.value.{Identifier, Record, RecordField, Value}
import com.daml.lf.data.Time.{Date, Timestamp}
import com.daml.script.dump.TreeUtils._
import org.typelevel.paiges.Doc
import scalaz.std.list._
import scalaz.std.set._
import scalaz.syntax.foldable._

object Encode {
  def encodeTransactionTreeStream(trees: Seq[TransactionTree]): Doc = {
    val parties = trees.toList.foldMap(partiesInTree(_))
    val partyMap = partyMapping(parties)
    val cids = trees.map(treeCids(_))
    val cidMap = cidMapping(cids)
    val refs = trees.toList.foldMap(treeRefs(_))
    val moduleRefs = refs.map(_.moduleName).toSet
    Doc.text("module Dump where") /
      Doc.text("import Daml.Script") /
      Doc.stack(moduleRefs.map(encodeImport(_))) /
      (Doc.text("dump = do") /
        Doc.stack(parties.map(p =>
          encodeParty(partyMap, p) + Doc.text(" <- allocateParty \"") + Doc.text(p) + Doc.text(
            "\""))) /
        Doc.stack(trees.map(t => encodeTree(partyMap, cidMap, t))) /
        Doc.text("pure ()")).hang(2)
  }

  private def encodeLocalDate(d: LocalDate): Doc = {
    val formatter = DateTimeFormatter.ofPattern("uuuu MMM d")
    Doc.text("(date ") + Doc.text(formatter.format(d)) + Doc.text(")")
  }

  private def encodeValue(
      partyMap: Map[String, String],
      cidMap: Map[String, String],
      v: Value.Sum): Doc = {
    def go(v: Value.Sum): Doc =
      v match {
        case Sum.Empty => throw new IllegalArgumentException("Empty value")
        case Sum.Record(value) => encodeRecord(partyMap, cidMap, value)
        case Sum.Variant(value) =>
          Doc.text("(") + qualifyId(value.getVariantId) +
            Doc.text(" ") + go(value.getValue.sum) + Doc.text(")")
        case Sum.ContractId(c) => encodeCid(cidMap, c)
        case Sum.List(value) =>
          Doc.text("[") + Doc.intercalate(
            Doc.text(", "),
            value.elements.map(v => go(v.sum))) +
            Doc.text("]")
        case Sum.Int64(i) => Doc.str(i)
        case Sum.Numeric(i) => Doc.str(i)
        case Sum.Text(t) =>
          // lol who needs escaping
          Doc.text("\"") + Doc.text(t) + Doc.text("\"")
        case Sum.Party(p) => encodeParty(partyMap, p)
        case Sum.Bool(b) =>
          Doc.text(if (b) {
            "True"
          } else "False")
        case Sum.Unit(_) => Doc.text("()")
        case Sum.Timestamp(micros) =>
          val t: ZonedDateTime = Timestamp.assertFromLong(micros).toInstant.atZone(ZoneId.of("UTC"))
          val formatter = DateTimeFormatter.ofPattern("H m s")
          Doc.text("(time ") + encodeLocalDate(t.toLocalDate) + Doc.text(" ") + Doc.text(formatter.format(t)) + Doc.text(")")
        case Sum.Date(daysSinceEpoch) =>
          val d = Date.assertFromDaysSinceEpoch(daysSinceEpoch)
          encodeLocalDate(LocalDate.ofEpochDay(d.days.toLong))
          case Sum.Optional(value) =>
          value.value match {
            case None => Doc.text("None")
            case Some(v) => Doc.text("(") + Doc.text("Some ") + go(v.sum) + Doc.text(")")
          }
        case Sum.Map(_) =>
          throw new RuntimeException("Maps are hard")
        case Sum.Enum(value) =>
          Doc.text(value.constructor)
        case Sum.GenMap(_) => throw new RuntimeException("generic maps are harder")
      }

  go(v)
  }

  private def encodeRecord(
      partyMap: Map[String, String],
      cidMap: Map[String, String],
      r: Record): Doc = {
    (qualifyId(r.getRecordId) + Doc.text(" with") /
      Doc.stack(r.fields.map(f => encodeField(partyMap, cidMap, f)))).nested(2)
  }

  private def encodeField(
      partyMap: Map[String, String],
      cidMap: Map[String, String],
      field: RecordField): Doc =
    Doc.text(field.label) + Doc.text(" = ") + encodeValue(partyMap, cidMap, field.getValue.sum)

  private def encodeParty(partyMap: Map[String, String], s: String): Doc = Doc.text(partyMap(s))

  private def encodeCid(cidMap: Map[String, String], cid: String): Doc = {
    // LedgerStrings are strings that match the regexp ``[A-Za-z0-9#:\-_/ ]+
    Doc.text(cidMap(cid))
  }

  private def qualifyId(id: Identifier): Doc =
    Doc.text(id.moduleName) + Doc.text(".") + Doc.text(id.entityName)

  private def encodeEv(
      partyMap: Map[String, String],
      cidMap: Map[String, String],
      ev: TreeEvent.Kind): Doc = ev match {
    case TreeEvent.Kind.Created(created) =>
      Doc.text("createCmd ") + encodeRecord(partyMap, cidMap, created.getCreateArguments)
    case TreeEvent.Kind.Exercised(exercised @ _) =>
      Doc.text("exerciseCmd ") + encodeCid(cidMap, exercised.contractId) + Doc.space + encodeValue(
        partyMap,
        cidMap,
        exercised.getChoiceArgument.sum)
    case TreeEvent.Kind.Empty => throw new IllegalArgumentException("Unknown tree event")
  }

  private def bindCid(cidMap: Map[String, String], c: CreatedContract): Doc = {
    Doc.text("let ") + encodeCid(cidMap, c.cid) + Doc.text(" = createdCid @") +
      qualifyId(c.tplId) + Doc.text(" $ ") + Doc.intercalate(
      Doc.text(" $ "),
      c.path.reverse.map(encodeCrumb(_))) + Doc.text(" tree")
  }

  private def encodeTree(
      partyMap: Map[String, String],
      cidMap: Map[String, String],
      tree: TransactionTree): Doc = {
    val rootEvs = tree.rootEventIds.map(tree.eventsById(_).kind)
    val submitters = rootEvs.flatMap(evParties(_)).toSet
    if (submitters.size != 1) {
      throw new IllegalArgumentException(s"Exactly one submitter is required but got $submitters")
    } else {
      val submitter = submitters.head
      val cids = treeCids(tree)
      (Doc.text("tree <- submitTree ") + encodeParty(partyMap, submitter) + Doc.text(" do") /
        Doc.stack(rootEvs.map(ev => encodeEv(partyMap, cidMap, ev)))).hang(2) /
        Doc.stack(cids.map(bindCid(cidMap, _)))
    }
  }

  private def encodeCrumb(crumb: Crumb): Doc = crumb match {
    case SelectRoot(i) => "selectRoot " +: Doc.str(i)
    case SelectChild(i) => "selectChild " +: Doc.str(i)
  }

  private def encodeImport(moduleName: String) =
    Doc.text("import qualified ") + Doc.text(moduleName)

  private def partyMapping(parties: Set[String]): Map[String, String] = {
    // - PartyIdStrings are strings that match the regexp ``[A-Za-z0-9:\-_ ]+``.
    def safeParty(p: String) =
      Seq(":", "-", "_", " ").foldLeft(p) { case (p, x) => p.replace(x, "") }.toLowerCase
    // Map from original party id to DAML identifier
    var partyMap: Map[String, String] = Map.empty
    // Number of times we’ve gotten the same result from safeParty, we resolve collisions with a suffix.
    var usedParties: Map[String, Int] = Map.empty
    parties.foreach { p =>
      val r = safeParty(p)
      usedParties.get(r) match {
        case None =>
          partyMap += p -> s"${r}_0"
          usedParties += r -> 0
        case Some(value) =>
          partyMap += p -> s"${r}_${value + 1}"
          usedParties += r -> (value + 1)
      }
    }
    partyMap
  }

  private def cidMapping(cids: Seq[Seq[CreatedContract]]): Map[String, String] = {
    def lowerFirst(s: String) =
      if (s.isEmpty) {
        s
      } else {
        s.head.toLower.toString + s.tail
      }
    cids.zipWithIndex.toList.flatMap {
      case (cs, treeIndex) =>
        cs.zipWithIndex.map {
          case (c, i) =>
            c.cid -> s"${lowerFirst(c.tplId.entityName)}_${treeIndex}_$i"
        }
    }.toMap
  }
}
