// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao.events

import java.sql.{Connection, PreparedStatement}
import java.time.Instant

import anorm.{BatchSql, NamedParameter, Row, SimpleSql, SqlStringInterpolation, ToStatement}
import com.daml.ledger._
import com.daml.ledger.participant.state.v1.{CommittedTransaction, Offset, SubmitterInfo}
import com.daml.platform.store.Conversions._

private[events] trait EventsTableInsert { this: EventsTable =>

  final class TransactionInfo(
      val submitterInfo: Option[SubmitterInfo],
      val workflowId: Option[WorkflowId],
      val transactionId: TransactionId,
      val ledgerEffectiveTime: Instant,
      val offset: Offset,
      val transaction: CommittedTransaction,
      val flatWitnesses: WitnessRelation[NodeId],
      val treeWitnesses: WitnessRelation[NodeId],
  )

  final class EventColumns(
      val eventIds: Array[String],
      val eventOffsets: Array[Array[Byte]],
      val contractIds: Array[String],
      val transactionIds: Array[String],
      val workflowIds: Array[String],
      val ledgerEffectiveTimes: Array[Instant],
      val templateIds: Array[String],
      val nodeIndexes: Array[String],
      val commandIds: Array[String],
      val applicationIds: Array[String],
      val submitters: Array[String],
      val flatEventWitnesses: Array[String],
      val treeEventWitnesses: Array[String],
      val createArguments: Array[Array[Byte]],
      val createSignatories: Array[String],
      val createObservers: Array[String],
      val createAgreementTexts: Array[String],
      val createConsumedAt: Array[Array[Byte]],
      val createKeyValues: Array[Array[Byte]],
      val exerciseConsuming: Array[String],
      val exerciseChoices: Array[String],
      val exerciseArguments: Array[Array[Byte]],
      val exerciseResults: Array[Array[Byte]],
      val exerciseActors: Array[String],
      val exerciseChildEventIds: Array[String],
  )

  object EventColumns {
    def apply(
        transactionInfo: TransactionInfo,
        lfValueTranslation: LfValueTranslation,
    ): EventColumns = {
      import transactionInfo._
      val tx = transaction.fold(Vector.empty[(NodeId, Node)]) {
        case (acc, n @ (_, _: Create)) => acc :+ n
        case (acc, n @ (_, _: Exercise)) => acc :+ n
        case (acc, _) => acc
      }
      val capacity = tx.size
      val e = new EventColumns(
        eventIds = Array.fill(capacity)(null.asInstanceOf[String]),
        eventOffsets = Array.fill(capacity)(offset.toByteArray),
        contractIds = Array.fill(capacity)(null.asInstanceOf[String]),
        transactionIds = Array.fill(capacity)(transactionId),
        workflowIds = Array.fill(capacity)(transactionInfo.workflowId.orNull),
        ledgerEffectiveTimes = Array.fill(capacity)(ledgerEffectiveTime),
        templateIds = Array.fill(capacity)(null.asInstanceOf[String]),
        nodeIndexes = Array.fill(capacity)(null.asInstanceOf[String]),
        commandIds = Array.fill(capacity)(submitterInfo.map(_.commandId).orNull),
        applicationIds = Array.fill(capacity)(submitterInfo.map(_.applicationId).orNull),
        submitters = Array.fill(capacity)(submitterInfo.map(_.singleSubmitterOrThrow).orNull),
        flatEventWitnesses = Array.fill(capacity)(null.asInstanceOf[String]),
        treeEventWitnesses = Array.fill(capacity)(null.asInstanceOf[String]),
        createArguments = Array.fill(capacity)(null.asInstanceOf[Array[Byte]]),
        createSignatories = Array.fill(capacity)(null.asInstanceOf[String]),
        createObservers = Array.fill(capacity)(null.asInstanceOf[String]),
        createAgreementTexts = Array.fill(capacity)(null.asInstanceOf[String]),
        createConsumedAt = Array.fill(capacity)(null.asInstanceOf[Array[Byte]]),
        createKeyValues = Array.fill(capacity)(null.asInstanceOf[Array[Byte]]),
        exerciseConsuming = Array.fill(capacity)(null.asInstanceOf[String]),
        exerciseChoices = Array.fill(capacity)(null.asInstanceOf[String]),
        exerciseArguments = Array.fill(capacity)(null.asInstanceOf[Array[Byte]]),
        exerciseResults = Array.fill(capacity)(null.asInstanceOf[Array[Byte]]),
        exerciseActors = Array.fill(capacity)(null.asInstanceOf[String]),
        exerciseChildEventIds = Array.fill(capacity)(null.asInstanceOf[String]),
      )
      var i = 0
      tx.foreach {
        case (nodeId, create: Create) =>
          val eventId = EventId(transactionId, nodeId)
          val (createArgument, createKeyValue) =
            lfValueTranslation.serializeRaw(eventId, create)
          e.contractIds(i) = create.coid.coid
          e.templateIds(i) = create.coinst.template.toString
          e.eventIds(i) = eventId.toLedgerString
          e.nodeIndexes(i) = nodeId.index.toString
          e.flatEventWitnesses(i) = flatWitnesses.getOrElse(nodeId, Set.empty).mkString("|")
          e.treeEventWitnesses(i) = treeWitnesses.getOrElse(nodeId, Set.empty).mkString("|")
          e.createArguments(i) = createArgument
          e.createSignatories(i) = create.signatories.mkString("|")
          e.createObservers(i) = create.stakeholders.diff(create.signatories).mkString("|")
          if (create.coinst.agreementText.nonEmpty) {
            e.createAgreementTexts(i) = create.coinst.agreementText
          }
          e.createKeyValues(i) = createKeyValue.orNull
          i += 1
        case (nodeId, exercise: Exercise) =>
          val eventId = EventId(transactionId, nodeId)
          val (exerciseArgument, exerciseResult) =
            lfValueTranslation.serializeRaw(eventId, exercise)
          e.contractIds(i) = exercise.targetCoid.coid
          e.templateIds(i) = exercise.templateId.toString
          e.eventIds(i) = eventId.toLedgerString
          e.nodeIndexes(i) = nodeId.index.toString
          e.flatEventWitnesses(i) = flatWitnesses.getOrElse(nodeId, Set.empty).mkString("|")
          e.treeEventWitnesses(i) = treeWitnesses.getOrElse(nodeId, Set.empty).mkString("|")
          e.exerciseConsuming(i) = exercise.consuming.toString
          e.exerciseChoices(i) = exercise.choiceId
          e.exerciseArguments(i) = exerciseArgument
          e.exerciseResults(i) = exerciseResult.orNull
          e.exerciseActors(i) = exercise.actingParties.mkString("|")
          e.exerciseChildEventIds(i) =
            exercise.children.map(EventId(transactionId, _).toLedgerString).iterator.mkString("|")
          i += 1
        case _ =>
        // skip everything else
      }
      e
    }
  }

  private implicit object StringArrayToStatement extends ToStatement[Array[String]] {
    override def set(s: PreparedStatement, index: Int, v: Array[String]): Unit = {
      s.setObject(index, v)
    }
  }

  private implicit object ByteArrayToStatement extends ToStatement[Array[Array[Byte]]] {
    override def set(s: PreparedStatement, index: Int, v: Array[Array[Byte]]): Unit = {
      s.setObject(index, v)
    }
  }

  private implicit object InstantArrayToStatement extends ToStatement[Array[Instant]] {
    override def set(s: PreparedStatement, index: Int, v: Array[Instant]): Unit = {
      val conn = s.getConnection
      val ts = conn.createArrayOf("TIMESTAMP", v.map(java.sql.Timestamp.from))
      s.setArray(index, ts)
    }
  }

  // create table foo(k int primary key, ts text[]);
  // insert into foo select k,string_to_array(ts,'|') from unnest('{1,2,3,42}'::int[], '{foo|bar,baz,"",null}'::text[]) as t(k,ts);
  private def insertEvent(columns: EventColumns): SimpleSql[Row] = {
    import columns._
    SQL"""insert into participant_events(
           event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, submitter, flat_event_witnesses, tree_event_witnesses,
           create_argument, create_signatories, create_observers, create_agreement_text, create_consumed_at, create_key_value,
           exercise_consuming, exercise_choice, exercise_argument, exercise_result, exercise_actors, exercise_child_event_ids
         )
         select
           event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, submitter, string_to_array(flat_event_witnesses, '|'), string_to_array(tree_event_witnesses, '|'),
           create_argument, string_to_array(create_signatories,'|'), string_to_array(create_observers,'|'), create_agreement_text, create_consumed_at, create_key_value,
           exercise_consuming, exercise_choice, exercise_argument, exercise_result, string_to_array(exercise_actors,'|'), string_to_array(exercise_child_event_ids,'|')
         from
           unnest(
             $eventIds::varchar[], $eventOffsets::bytea[], $contractIds::varchar[], $transactionIds::varchar[], $workflowIds::varchar[], $ledgerEffectiveTimes::timestamp[], $templateIds::varchar[], $nodeIndexes::varchar[], $commandIds::varchar[], $applicationIds::varchar[], $submitters::varchar[], $flatEventWitnesses::varchar[], $treeEventWitnesses::varchar[],
             $createArguments::bytea[], $createSignatories::varchar[], $createObservers::varchar[], $createAgreementTexts::varchar[], $createConsumedAt::bytea[], $createKeyValues::bytea[],
             $exerciseConsuming::varchar[], $exerciseChoices::varchar[], $exerciseArguments::bytea[], $exerciseResults::bytea[], $exerciseActors::varchar[], $exerciseChildEventIds::varchar[]
           )
           as
               t(
                 event_id, event_offset, contract_id, transaction_id, workflow_id, ledger_effective_time, template_id, node_index, command_id, application_id, submitter, flat_event_witnesses, tree_event_witnesses,
                 create_argument, create_signatories, create_observers, create_agreement_text, create_consumed_at, create_key_value,
                 exercise_consuming, exercise_choice, exercise_argument, exercise_result, exercise_actors, exercise_child_event_ids
               )
       """
  }

  private val updateArchived =
    """update participant_events set create_consumed_at={consumed_at} where contract_id={contract_id} and create_argument is not null"""

  private def archive(
      contractId: ContractId,
      consumedAt: Offset,
  ): Vector[NamedParameter] =
    Vector[NamedParameter](
      "consumed_at" -> consumedAt,
      "contract_id" -> contractId.coid,
    )

  final class PreparedBatches(
      events: TransactionInfo,
      archives: Option[BatchSql],
  ) {
    def serialize(lfValueTranslation: LfValueTranslation): SerializedBatches =
      new SerializedBatches(
        insertEvent(EventColumns(events, lfValueTranslation)),
        archives,
      )
  }

  final class SerializedBatches(
      val events: SimpleSql[Row],
      archives: Option[BatchSql],
  ) {
    def isEmpty: Boolean = archives.isEmpty
    def execute()(implicit connection: Connection): Unit = {
      events.execute()
      archives.foreach(_.execute())
    }
  }

  private case class AccumulatingBatches(
      archives: Vector[Vector[NamedParameter]],
  ) {

    def add(archive: Vector[NamedParameter]): AccumulatingBatches =
      copy(archives = archives :+ archive)

    private def prepareNonEmpty(
        query: String,
        params: Vector[Vector[NamedParameter]],
    ): Option[BatchSql] =
      if (params.nonEmpty) Some(BatchSql(query, params.head, params.tail: _*)) else None

    def prepare(
        submitterInfo: Option[SubmitterInfo],
        workflowId: Option[WorkflowId],
        transactionId: TransactionId,
        ledgerEffectiveTime: Instant,
        offset: Offset,
        transaction: CommittedTransaction,
        flatWitnesses: WitnessRelation[NodeId],
        treeWitnesses: WitnessRelation[NodeId],
    ): PreparedBatches =
      new PreparedBatches(
        new TransactionInfo(
          submitterInfo,
          workflowId,
          transactionId,
          ledgerEffectiveTime,
          offset,
          transaction,
          flatWitnesses,
          treeWitnesses,
        ),
        prepareNonEmpty(updateArchived, archives),
      )

  }

  private object AccumulatingBatches {
    val empty: AccumulatingBatches = AccumulatingBatches(
      archives = Vector.empty,
    )
  }

  /**
    * @throws RuntimeException If a value cannot be serialized into an array of bytes
    */
  @throws[RuntimeException]
  def prepareBatchInsert(
      submitterInfo: Option[SubmitterInfo],
      workflowId: Option[WorkflowId],
      transactionId: TransactionId,
      ledgerEffectiveTime: Instant,
      offset: Offset,
      transaction: CommittedTransaction,
      flatWitnesses: WitnessRelation[NodeId],
      treeWitnesses: WitnessRelation[NodeId],
  ): PreparedBatches = {

    transaction
      .fold(AccumulatingBatches.empty) {
        case (batches, (_, node: Exercise)) if node.consuming =>
          batches.add(
            archive(
              contractId = node.targetCoid,
              consumedAt = offset,
            )
          )
        case (batches, _) =>
          batches // ignore any event which is neither a create nor an exercise
      }
      .prepare(
        submitterInfo,
        workflowId,
        transactionId,
        ledgerEffectiveTime,
        offset,
        transaction,
        flatWitnesses,
        treeWitnesses,
      )
  }

}
