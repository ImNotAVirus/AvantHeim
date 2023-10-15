defmodule GameService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  alias GameService.ConfigFile

  ## Application behaviour

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    # FIXME: Remove hash and use event.partition
    partition_hash = fn event -> {event, event.partition} end

    children = [
      {Cluster.Supervisor, [topologies, [name: GameService.ClusterSupervisor]]},
      {ElvenGard.Cluster.MnesiaClusterManager, []},
      {ElvenGard.ECS.Topology.EventSource, [hash: partition_hash]},
      {DynamicSupervisor, strategy: :one_for_one, name: static_map_supervisor()},
      # Start a Partition with the ":system" id
      {GameService.SystemPartition, []},
      # Start a partition for each static maps
      {Task, &start_map_partitions/0}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GameService.Supervisor]
    Supervisor.start_link(children, opts)
  end

  ## Helpers

  defp static_map_supervisor(), do: GameService.StaticMapSupervisor

  defp start_map_partitions() do
    Enum.each(ConfigFile.static_map_ids(), fn map_id ->
      {:ok, _} =
        DynamicSupervisor.start_child(
          static_map_supervisor(),
          {GameService.StaticMapPartition, [id: map_id]}
        )
    end)
  end
end
