// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.on.sql

import java.time.Clock

import com.daml.ledger.participant.state.kvutils.ParticipantStateIntegrationSpecBase
import com.daml.ledger.participant.state.kvutils.api.KeyValueParticipantState
import com.daml.ledger.participant.state.v1._
import com.digitalasset.daml.lf.data.Ref.LedgerString
import com.digitalasset.daml.lf.data.Time.Timestamp
import com.digitalasset.logging.LoggingContext.newLoggingContext

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}
import scala.util.Random

class H2MemorySqlLedgerReaderWriterIntegrationSpec
    extends ParticipantStateIntegrationSpecBase("SQL implementation using H2 in memory") {
  private implicit val ec: ExecutionContext = ExecutionContext.global

  override val firstIndex: Long = SqlLedgerReaderWriter.FirstIndex

  override def participantStateFactory(
      participantId: ParticipantId,
      ledgerId: LedgerString,
  ): ReadService with WriteService with AutoCloseable = {
    val databaseName = s"${getClass.getSimpleName.toLowerCase()}_${Random.nextInt()}"
    val jdbcUrl = s"jdbc:h2:mem:$databaseName"
    newLoggingContext { implicit logCtx =>
      val readerWriter =
        Await.result(SqlLedgerReaderWriter(ledgerId, participantId, jdbcUrl), 10.seconds)
      new KeyValueParticipantState(readerWriter, readerWriter)
    }
  }

  override def currentRecordTime(): Timestamp =
    Timestamp.assertFromInstant(Clock.systemUTC().instant())
}