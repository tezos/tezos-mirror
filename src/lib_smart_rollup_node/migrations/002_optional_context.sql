--------------------------------------------------------
-- SPDX-License-Identifier: MIT                       --
-- Copyright (c) 2024 Functori <contact@functori.com> --
--------------------------------------------------------

-- No way to drop constraints on columns in sqlite
ALTER TABLE l2_blocks ADD COLUMN context2 VARCHAR(54);
UPDATE l2_blocks SET context2 = context;
ALTER TABLE l2_blocks DROP COLUMN context;
ALTER TABLE l2_blocks RENAME COLUMN context2 TO context;
