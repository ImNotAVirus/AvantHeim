defmodule GameService.SystemPartition do
  @moduledoc """
  TODO: Documentation for GameService.StaticMapPartition
  """

  use ElvenGard.ECS.Topology.Partition, restart: :permanent

  # Partition behaviour

  @impl true
  def setup(_opts) do
    # Run system 60 per seconds (60Hz)
    interval = trunc(1 / 60 * 1000)

    {:system, systems: systems(), interval: interval}
  end

  # Private functions

  defp systems() do
    [
      GameService.SystemPartition.PlayerSpawnSystem
    ]
  end
end
