defmodule LoginService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {ElvenCaching.SessionRegistry, []},
      {LoginService.Endpoint, name: LoginService.Endpoint}
    ]

    opts = [strategy: :one_for_one, name: LoginService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
