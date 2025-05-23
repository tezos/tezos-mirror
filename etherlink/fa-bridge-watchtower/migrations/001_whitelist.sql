-----------------------------------------------------------------
-- SPDX-License-Identifier: MIT                                --
-- Copyright (c) 2025 Functori <contact@functori.com>          --
-- Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>  --
-----------------------------------------------------------------

CREATE INDEX idx_proxy ON deposits(proxy);
CREATE INDEX idx_ticket_hash ON deposits(ticket_hash);
CREATE INDEX idx_receiver ON deposits(receiver);

CREATE TABLE whitelist (
  proxy BLOB,
  ticket_hash BLOB
);

CREATE INDEX wid_proxy ON whitelist(proxy);
CREATE INDEX wid_ticket_hash ON whitelist(ticket_hash);
