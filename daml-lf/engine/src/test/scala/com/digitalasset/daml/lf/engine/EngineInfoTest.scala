// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine

import org.scalatest.{Matchers, WordSpec}

class EngineInfoTest extends WordSpec with Matchers {

  "EngineInfo" should {

    def infos = Seq(EngineConfig.Stable, EngineConfig.Dev).map(new EngineInfo(_))

    val Seq(engineInfoStable, engineInfoDev) = infos

    "show supported LF, Transaction and Value versions" in {

      infos.foreach(
        _.pretty.toSeq(0) shouldBe
          "DAML LF Engine supports LF versions: 1.6, 1.7, 1.8, 1.dev; input transaction versions: 10, 11; input value versions: 6, 7; output transaction versions: 10, 11; output value versions: 6, 7."
      )
    }

    "show allowed LF, Transaction and Value versions" in {

      engineInfoStable.pretty.toSeq(1) shouldBe
        "DAML LF Engine config allows LF versions: 1.6, 1.7, 1.8; input transaction versions: 10; input value versions: 6; output transaction versions: 10; output value versions: 6."

      engineInfoDev.pretty.toSeq(1) shouldBe
        "DAML LF Engine config allows LF versions: 1.6, 1.7, 1.8, 1.dev; input transaction versions: 10, 11; input value versions: 6, 7; output transaction versions: 10, 11; output value versions: 6, 7."

    }

    "toString returns the same value as show" in {
      engineInfoStable.toString shouldBe engineInfoStable.show
    }
  }
}
