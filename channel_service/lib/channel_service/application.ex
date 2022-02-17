defmodule ChannelService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      {Cluster.Supervisor, [topologies, [name: LoginService.ClusterSupervisor]]},
      {ElvenCaching.MnesiaClusterManager, []},
      {ElvenCaching.SessionRegistry, []},
      {ChannelService.Endpoint, name: ChannelService.Endpoint}
    ]

    opts = [strategy: :one_for_all, name: ChannelService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
