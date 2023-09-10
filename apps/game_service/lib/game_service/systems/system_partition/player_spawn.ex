defmodule GameService.SystemPartition.PlayerSpawnSystem do
  use ElvenGard.ECS.System,
    lock_components: :sync,
    event_subscriptions: [ElvenGard.ECS.Events.EntitySpawned]

  alias ElvenGard.ECS.Entity
  alias ElvenGard.ECS.Query
  alias ElvenGard.ECS.Events.EntitySpawned
  alias GameService.PlayerComponents.EndpointComponent
  alias GameService.EntityComponents.PositionComponent

  # System behaviour

  @impl true
  def run(%EntitySpawned{entity: entity, components: components}, _delta) do
    components
    |> Enum.find(&match?(%PositionComponent{}, &1))
    |> then(&maybe_broadcast_spawn(entity, components, &1))
  end

  ## Helpers

  # defp maybe_broadcast_spawn(_entity, _components, nil), do: :ok

  defp maybe_broadcast_spawn(entity, components, %PositionComponent{map_ref: map_ref}) do
    players =
      Query.select(
        Entity,
        with: [{PositionComponent, [{:==, :map_ref, map_ref}]}],
        preload: [EndpointComponent]
      )
      |> Query.all()

    IO.inspect(players, label: "players")
    GameService.broadcast_to({:entity_spawn, entity, components}, players)
  end
end
