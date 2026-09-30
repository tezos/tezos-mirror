CREATE INDEX block_tez_hash_index ON blocks (tez_hash) WHERE tez_hash IS NOT NULL;
