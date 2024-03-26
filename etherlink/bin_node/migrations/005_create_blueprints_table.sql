CREATE TABLE blueprints (
  id SERIAL PRIMARY KEY,
  payload BLOB NOT NULL,
  timestamp DATETIME NOT NULL
);

DROP TABLE executable_blueprints;

DROP TABLE publishable_blueprints;

CREATE TABLE delayed_transactions (
  injected_before INT NOT NULL,
  hash TEXT NOT NULL,
  payload TEXT NOT NULL
);
