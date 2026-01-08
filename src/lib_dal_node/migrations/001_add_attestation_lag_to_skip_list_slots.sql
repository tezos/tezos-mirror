CREATE TABLE skip_list_slots_new (
  published_level INTEGER NOT NULL,
  slot_index INTEGER NOT NULL,
  attestation_lag INTEGER NOT NULL,
  skip_list_cell_hash VARCHAR NOT NULL,
  PRIMARY KEY(published_level, slot_index)
);

INSERT INTO skip_list_slots_new (published_level, slot_index, attestation_lag, skip_list_cell_hash)
SELECT attested_level - 8, slot_index, 8, skip_list_cell_hash
FROM skip_list_slots;

DROP TABLE skip_list_slots;

ALTER TABLE skip_list_slots_new RENAME TO skip_list_slots;
