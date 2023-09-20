defmodule GameService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    # FIXME: Remove hash and use event.partition
    partition_hash = fn event -> {event, event.partition} end

    children = [
      {Cluster.Supervisor, [topologies, [name: GameService.ClusterSupervisor]]},
      {ElvenGard.Cluster.MnesiaClusterManager, []},
      {ElvenGard.ECS.Topology.EventSource, [hash: partition_hash]},
      # FIXME: Later rewrite partitions with a DynamicSupervisor
      {GameService.StaticMapPartition, [id: 1]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GameService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
