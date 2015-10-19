CREATE TABLE doge (
  id   SERIAL PRIMARY KEY
);

CREATE TABLE doge_name (
  id        SERIAL PRIMARY KEY,
  doge_id   INTEGER REFERENCES doge(id),
  doge_name TEXT NOT NULL
);

CREATE TABLE wallet (
  id      SERIAL PRIMARY KEY,
  doge_id INTEGER REFERENCES doge(id),
  coins   INTEGER NOT NULL CHECK (coins >= 0)
);

CREATE TABLE pack (
  id        SERIAL PRIMARY KEY,
  pack_name TEXT NOT NULL
);

CREATE TABLE pack_member (
  id      SERIAL PRIMARY KEY,
  pack_id INTEGER REFERENCES pack(id),
  doge_id INTEGER REFERENCES doge(id)
);
