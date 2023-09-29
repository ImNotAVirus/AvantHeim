defmodule GameService.SystemPartition do
  @moduledoc """
  TODO: Documentation for GameService.SystemPartition
  """

  use ElvenGard.ECS.Topology.Partition

  require Logger

  # Partition behaviour

  @impl true
  def setup(_opts) do
    Logger.debug("SystemPartition is starting...")

    # Run system 60 per seconds (60Hz)
    interval = trunc(1 / 60 * 1000)

    {:system, systems: systems(), interval: interval}
  end

  # Private functions

  defp systems() do
    [
      GameService.SessionDisconnectionSystem,
      GameService.PulseSystem
    ]
  end
end
