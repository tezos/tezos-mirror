CREATE TABLE l1_latest_level (
  lock CHAR(1) PRIMARY KEY NOT NULL DEFAULT 'l',
  level INT,
  -- The CONSTRAINT allows no more than 1 row in this table, making sure we
  -- only have 1 tezos level at most
  CONSTRAINT CK_T1_Locked CHECK (lock='l')
)
