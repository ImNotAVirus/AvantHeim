defmodule MapService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      {Cluster.Supervisor, [topologies, [name: MapService.ClusterSupervisor]]},
      {ElvenCaching.MnesiaClusterManager, []},
      {ElvenCaching.CharacterRegistry, []},
      {MapService.MapSupervisor, []}
    ]

    opts = [strategy: :rest_for_one, name: MapService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
