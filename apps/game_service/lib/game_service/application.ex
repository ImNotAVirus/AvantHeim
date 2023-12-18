defmodule GameService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  require Logger

  alias GameService.GameConfig
  alias GameService.TelemetryForwarder
  alias ElvenGard.ECS.Topology

  ## Application behaviour

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    # Setup Telemetry to forward events to the admin service
    :ok = TelemetryForwarder.setup()

    # Init and populate ETS tables
    Logger.info("Init GameService...")
    # FIXME: OTP 26: use :milisecond instead of /1000
    {time, stats} = :timer.tc(&GameConfig.init/0)
    time = Float.round(time / 1000, 1)
    Logger.info("Initialization done (#{time}ms): #{build_debug_stats(stats)}")

    # FIXME: Remove hash and use event.partition
    partition_hash = fn event -> {event, event.partition} end

    children = [
      {Cluster.Supervisor, [topologies, [name: GameService.ClusterSupervisor]]},
      {ElvenGard.Cluster.MnesiaClusterManager, []},
      {ElvenGard.ECS.Topology.EventSource, [hash: partition_hash]},
      {DynamicSupervisor, strategy: :one_for_one, name: static_map_supervisor()},
      # Start a Partition with the ":system" id
      {GameService.SystemPartition, []},
      # Load configs and start a partition for each static maps
      {Task, &start_map_partitions/0}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GameService.Supervisor]
    Supervisor.start_link(children, opts)
  end

  ## Helpers

  defp static_map_supervisor(), do: GameService.StaticMapSupervisor

  defp build_debug_stats(stats) do
    stats |> Enum.map(&"#{elem(&1, 1)} #{elem(&1, 0)}") |> Enum.join(" - ")
  end

  defp start_map_partitions() do
    start = ElvenGard.ECS.now()

    Logger.info("Starting maps...")

    static_pids =
      Enum.map(GameConfig.static_map_info_ids(), fn map_id ->
        {:ok, pid} =
          DynamicSupervisor.start_child(
            static_map_supervisor(),
            {GameService.StaticMapPartition, [id: map_id]}
          )

        pid
      end)

    # FIXME: Support for :infinity
    :ok = Topology.wait_for_partitions(static_pids, 10000)
    time = ElvenGard.ECS.now() - start

    Logger.info("#{length(static_pids)} static maps loaded (#{time}ms)")
  end
end
