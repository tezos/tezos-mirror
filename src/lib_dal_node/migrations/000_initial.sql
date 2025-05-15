----------------------------------------------------------------
-- SPDX-License-Identifier: MIT                               --
-- Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com> --
----------------------------------------------------------------

-- sqlite does not take into account the VARCHAR size
-- https://www.sqlite.org/faq.html#q9

CREATE TABLE skip_list_slots (
  attested_level INTEGER NOT NULL,
  slot_index INTEGER NOT NULL,
  skip_list_cell_hash VARCHAR NOT NULL,
  PRIMARY KEY(attested_level, slot_index)
);

CREATE TABLE skip_list_cells (
  hash VARCHAR NOT NULL PRIMARY KEY,
  cell VARCHAR NOT NULL
);
