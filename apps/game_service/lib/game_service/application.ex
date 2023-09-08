defmodule GameService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    # Just send all events to the map with id 1 (NosVille) for now
    partition_hash = fn event -> {event, 1} end

    children = [
      {Cluster.Supervisor, [topologies, [name: GameService.ClusterSupervisor]]},
      {ElvenGard.ECS.Topology.EventSource, [hash: partition_hash]},
      {GameService.StaticMapPartition, [id: 1]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GameService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
