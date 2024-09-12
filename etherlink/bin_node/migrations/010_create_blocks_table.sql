CREATE TABLE blocks (
  level serial PRIMARY KEY,
  hash VARCHAR(32) NOT NULL,
  block BLOB NOT NULL
);
