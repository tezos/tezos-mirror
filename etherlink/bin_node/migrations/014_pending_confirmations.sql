CREATE TABLE pending_confirmations (
    level serial PRIMARY KEY,
    hash VARCHAR(32) NOT NULL
);
