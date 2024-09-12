CREATE TABLE transactions (
  block_hash VARCHAR(32) NOT NULL,
  block_number serial NOT NULL,
  index_ serial NOT NULL,
  hash VARCHAR(32) PRIMARY KEY NOT NULL,
  from_ VARCHAR(20) NOT NULL,
  to_ VARCHAR(20),
  receipt_fields BLOB NOT NULL,
  object_fields BLOB NOT NULL
);
