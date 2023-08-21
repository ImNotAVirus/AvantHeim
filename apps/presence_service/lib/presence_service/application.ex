defmodule PresenceService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {PresenceService.Registry, []},
      {DynamicSupervisor, strategy: :one_for_one, name: PresenceService.SessionSupervisor}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_all, name: PresenceService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
