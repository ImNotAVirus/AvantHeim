defmodule DatabaseService.Repo do
  use Ecto.Repo,
    otp_app: :database_service,
    adapter: Ecto.Adapters.Postgres
end
