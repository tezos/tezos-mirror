--------------------------------------------------------
-- SPDX-License-Identifier: MIT                       --
-- Copyright (c) 2026 Functori <contact@functori.com> --
--------------------------------------------------------

ALTER TABLE l2_blocks ADD COLUMN state_hash VARCHAR(54);
ALTER TABLE l2_blocks ADD COLUMN pvm_status BLOB;
