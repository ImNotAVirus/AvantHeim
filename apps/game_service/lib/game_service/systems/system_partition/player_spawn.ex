defmodule GameService.SystemPartition.PlayerSpawnSystem do
  use ElvenGard.ECS.System,
    lock_components: :sync,
    event_subscriptions: [ElvenGard.ECS.Events.EntitySpawned]

  alias ElvenGard.ECS.Events.EntitySpawned

  # System behaviour

  @impl true
  def run(%EntitySpawned{entity: entity}, delta) do
    IO.puts("[PlayerSpawnSystem] delta: #{delta} - entity: #{inspect(entity)}")
  end
end
