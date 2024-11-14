--------------------------------------------------------
-- SPDX-License-Identifier: MIT                       --
-- Copyright (c) 2024 Functori <contact@functori.com> --
--------------------------------------------------------

-- For storing levels such as LCC, LPC, GC levels, etc.
-- and information about the head, finalized_level, etc.
CREATE TABLE rollup_node_state (
  name VARCHAR PRIMARY KEY,
  value VARCHAR,
  level INTEGER
);

CREATE TABLE commitments (
  hash VARCHAR(54) PRIMARY KEY,
  compressed_state VARCHAR(54) NOT NULL,
  inbox_level INTEGER NOT NULL,
  predecessor VARCHAR(54) NOT NULL,
  number_of_ticks INTEGER NOT NULL
);

CREATE INDEX idx_commitments_inbox_level
ON commitments(inbox_level);

CREATE TABLE commitments_published_at_levels (
  commitment_hash VARCHAR(54) PRIMARY KEY,
  first_published_at_level INTEGER NOT NULL,
  published_at_level INTEGER
);

CREATE TABLE inboxes (
  hash VARCHAR(55) PRIMARY KEY,
  inbox_level INTEGER NOT NULL,
  history_proof BLOB NOT NULL
);

CREATE INDEX idx_inboxes_inbox_level
ON inboxes(inbox_level);

CREATE TABLE messages (
  payload_hashes_hash VARCHAR(55) PRIMARY KEY,
  inbox_level INTEGER NOT NULL,
  message_list BLOB NOT NULL
);

CREATE INDEX idx_messages_inbox_level
ON messages(inbox_level);

CREATE TABLE outbox_messages (
  outbox_level INTEGER PRIMARY KEY,
  messages VARCHAR NOT NULL,
  executed_messages VARCHAR NOT NULL
);

CREATE INDEX idx_outbox_messages_outbox_level
ON outbox_messages(outbox_level);

CREATE TABLE protocols (
  hash VARCHAR(51) PRIMARY KEY,
  proto_level INTEGER NOT NULL,
  first_level INTEGER NOT NULL,
  first_is_activation BOOLEAN NOT NULL
);

CREATE INDEX idx_protocols_first_level
ON protocols(first_level);

CREATE TABLE dal_slots_headers (
  block_hash VARCHAR(51) NOT NULL,
  slot_index INTEGER NOT NULL,
  published_level INTEGER NOT NULL,
  slot_commitment VARCHAR NOT NULL,
  PRIMARY KEY(block_hash, slot_index)
);

CREATE TABLE dal_slots_statuses (
  block_hash VARCHAR(51) NOT NULL,
  slot_index INTEGER NOT NULL,
  attested BOOLEAN NOT NULL,
  PRIMARY KEY(block_hash, slot_index)
);

CREATE TABLE l2_blocks (
  block_hash VARCHAR(51) PRIMARY KEY,
  level INTEGER NOT NULL,
  predecessor VARCHAR(51) NOT NULL ,
  commitment_hash VARCHAR(54) ,
  previous_commitment_hash VARCHAR(54) NOT NULL,
  context VARCHAR(54) NOT NULL,
  inbox_witness VARCHAR(55) NOT NULL,
  inbox_hash VARCHAR(55) NOT NULL ,
  initial_tick VARCHAR NOT NULL,
  num_ticks INTEGER NOT NULL
);

CREATE INDEX idx_l2_blocks_level
ON l2_blocks(level);

CREATE TABLE l2_levels (
  level INTEGER PRIMARY KEY,
  block_hash VARCHAR(51) NOT NULL
);
