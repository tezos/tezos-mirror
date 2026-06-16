-----------------------------------------------------------------
-- SPDX-License-Identifier: MIT                                --
-- Copyright (c) 2026 Functori <contact@functori.com>          --
-- Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>  --
-----------------------------------------------------------------

ALTER TABLE deposits
ADD COLUMN native_deposit BOOLEAN NOT NULL DEFAULT false;
