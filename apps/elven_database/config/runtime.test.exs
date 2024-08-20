import Config

config :elven_database, ElvenDatabase.Repo,
  database: "elvengard_test",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox
