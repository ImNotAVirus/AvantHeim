defmodule ChannelEndpoint.Application do
  @moduledoc false

  use Application

  alias ChannelEndpoint.Application.MapSupervisor

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: ChannelEndpoint.MapRegistry},
      {MapSupervisor, name: ChannelEndpoint.MapSupervisor},
      {ChannelEndpoint.EndpointManager, []},
      {ChannelEndpoint.Endpoint, []}
    ]

    opts = [strategy: :one_for_all, name: ChannelEndpoint.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
