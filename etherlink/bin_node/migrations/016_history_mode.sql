ALTER TABLE metadata RENAME TO old_metadata;

CREATE TABLE metadata (
  key TEXT PRIMARY KEY NOT NULL,
  value TEXT NOT null
);

INSERT INTO metadata (key, value)
SELECT 'smart_rollup_address', smart_rollup_address
FROM old_metadata;

DROP TABLE old_metadata;
