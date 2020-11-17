-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

ALTER TABLE participant_events ALTER COLUMN node_index TYPE varchar;
ALTER TABLE participant_events ALTER COLUMN exercise_consuming TYPE varchar;
