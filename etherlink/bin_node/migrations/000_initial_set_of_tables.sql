CREATE TABLE executable_blueprints (
  id SERIAL PRIMARY KEY,
  payload BLOB NOT NULL
);

CREATE TABLE publishable_blueprints (
  id SERIAL PRIMARY KEY,
  payload BLOB NOT NULL
);

CREATE TABLE context_hashes (
  id SERIAL PRIMARY KEY,
  context_hash VARCHAR(52) NOT NULL
);
