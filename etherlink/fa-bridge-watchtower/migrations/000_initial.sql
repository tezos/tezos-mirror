-----------------------------------------------------------------
-- SPDX-License-Identifier: MIT                                --
-- Copyright (c) 2025 Functori <contact@functori.com>          --
-- Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>  --
-----------------------------------------------------------------

CREATE TABLE deposits (
  nonce INTEGER PRIMARY KEY,
  proxy BLOB NOT NULL,
  ticket_hash BLOB NOT NULL,
  receiver BLOB NOT NULL,
  amount VARCHAR NOT NULL,
  log_transactionHash BLOB NOT NULL,
  log_transactionIndex INTEGER NOT NULL,
  log_logIndex INTEGER NOT NULL,
  log_blockHash BLOB NOT NULL,
  log_blockNumber INTEGER NOT NULL,
  log_removed BOOLEAN NOT NULL DEFAULT false,
  exec_transactionHash BLOB,
  exec_transactionIndex INTEGER,
  exec_blockHash BLOB,
  exec_blockNumber INTEGER
);

CREATE TABLE pointers (
  name varchar VARCHAR PRIMARY KEY,
  value INTEGER NOT NULL
);
