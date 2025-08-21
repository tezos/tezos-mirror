-----------------------------------------------------------------
-- SPDX-License-Identifier: MIT                                --
-- Copyright (c) 2025 Functori <contact@functori.com>          --
-- Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>  --
-----------------------------------------------------------------

CREATE TABLE withdrawals (
  transactionHash BLOB NOT NULL,
  transactionIndex INTEGER NOT NULL,
  logIndex INTEGER NOT NULL,
  blockHash BLOB NOT NULL,
  blockNumber INTEGER NOT NULL,
  removed BOOLEAN NOT NULL DEFAULT false,
  kind INTEGER NOT NULL,
  amount VARCHAR NOT NULL,
  sender BLOB NOT NULL,
  receiver VARCHAR NOT NULL,
  withdrawal_id VARCHAR NOT NULL,
  ticket_owner BLOB,
  outbox_level INTEGER,
  outbox_message_index INTEGER,
  outbox_transaction_index INTEGER,
  PRIMARY KEY(transactionHash, transactionIndex, logIndex)
);

CREATE TABLE pointers (
  name varchar VARCHAR PRIMARY KEY,
  value INTEGER NOT NULL
);

CREATE TABLE levels (
  l1 INTEGER PRIMARY KEY,
  start_l2 INTEGER NOT NULL,
  end_l2 INTEGER NOT NULL
);

CREATE INDEX idx_start_l2_levels ON levels(start_l2);
CREATE INDEX idx_end_l2_levels ON levels(end_l2);

CREATE TABLE outbox_messages (
  outbox_level INTEGER NOT NULL,
  message_index INTEGER NOT NULL,
  transaction_index INTEGER NOT NULL,
  destination VARCHAR NOT NULL,
  entrypoint VARCHAR NOT NULL,
  parameters VARCHAR NOT NULL,
  PRIMARY KEY(outbox_level, message_index, transaction_index)
);

CREATE TABLE executions (
  outbox_level INTEGER NOT NULL,
  message_index INTEGER NOT NULL,
  operation_hash VARCHAR NOT NULL,
  l1_block INTEGER NOT NULL,
  timestamp INTEGER NOT NULL,
  PRIMARY KEY(outbox_level, message_index)
);
