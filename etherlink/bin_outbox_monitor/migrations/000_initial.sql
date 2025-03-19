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
