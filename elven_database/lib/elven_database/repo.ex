defmodule ElvenDatabase.Repo do
  use Ecto.Repo,
    otp_app: :elven_database,
    adapter: Ecto.Adapters.Postgres
end
