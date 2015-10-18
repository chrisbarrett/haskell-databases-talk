-- -*- sql-product: postgres; -*-

CREATE TABLE doges (
  id   SERIAL PRIMARY KEY
);

CREATE TABLE doge_names (
  id        SERIAL PRIMARY KEY,
  doge_id   INTEGER REFERENCES doges(id),
  doge_name TEXT NOT NULL
);

CREATE TABLE wallets (
  id      SERIAL PRIMARY KEY,
  doge_id INTEGER REFERENCES doges(id),
  coins   INTEGER NOT NULL CHECK (coins >= 0)
);

CREATE TABLE packs (
  id        SERIAL PRIMARY KEY,
  pack_name TEXT NOT NULL
);

CREATE TABLE pack_members (
  id      SERIAL PRIMARY KEY,
  pack_id INTEGER REFERENCES packs(id),
  doge_id INTEGER REFERENCES doges(id)
);
