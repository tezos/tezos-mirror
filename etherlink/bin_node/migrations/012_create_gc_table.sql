CREATE TABLE gc (
  id INTEGER PRIMARY KEY,
  last_gc_level INTEGER,
  last_gc_timestamp TIMESTAMP,
  last_split_level INTEGER,
  last_split_timestamp TIMESTAMP
)
