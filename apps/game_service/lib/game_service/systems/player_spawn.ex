defmodule GameService.PlayerSpawnSystem do
  use ElvenGard.ECS.System,
    lock_components: :sync,
    event_subscriptions: [GameService.Events.EntitySpawned]

  alias ElvenGard.ECS.Query
  alias GameService.Events.EntitySpawned
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
    # Get all endpoints on the current map
    endpoints =
      Query.select(
        EndpointComponent,
        with: [{PositionComponent, [{:==, :map_ref, map_ref}]}]
      )
      |> Query.all()

    # Transform the entity + components to a bundle
    bundle = GameService.load_bundle(entity, components)

    # Broadcast the entity spawn to players
    GameService.broadcast_to({:entity_spawn, bundle}, endpoints)
  end
end
