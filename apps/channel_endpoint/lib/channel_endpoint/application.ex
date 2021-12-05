defmodule ChannelEndpoint.Application do
  @moduledoc false

  use Application

  alias ChannelEndpoint.Application.MapSupervisor

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: ChannelEndpoint.MapRegistry},
      {ChannelEndpoint.Endpoint, name: ChannelEndpoint.Endpoint},
      {MapSupervisor, name: ChannelEndpoint.MapSupervisor}
    ]

    opts = [strategy: :one_for_one, name: ChannelEndpoint.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
