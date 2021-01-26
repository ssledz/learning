CREATE SCHEMA app;

ALTER SCHEMA app OWNER TO test;

CREATE SEQUENCE app.users_id_seq START 1;

CREATE TABLE app.users (
  id bigint PRIMARY KEY DEFAULT nextval('app.users_id_seq'),
  user_name varchar NOT NULL UNIQUE,
  first_name varchar NOT NULL,
  last_name varchar NOT NULL,
  email varchar NOT NULL
);
