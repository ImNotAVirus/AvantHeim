defmodule LoginEndpoint.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {LoginEndpoint.Endpoint, name: LoginEndpoint.Endpoint}
    ]

    opts = [strategy: :one_for_one, name: LoginEndpoint.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
