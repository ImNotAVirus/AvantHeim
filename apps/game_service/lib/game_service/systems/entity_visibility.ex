defmodule GameService.EntityVisibilitySystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityVisibilitySystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use ElvenGard.ECS.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.EntitySpawned,
      GameService.Events.EntityDespawn
    ]

  alias ElvenGard.ECS.Query
  alias GameService.Events.{EntitySpawned, EntityDespawn}
  alias GameService.PlayerComponents.EndpointComponent
  alias GameService.EntityComponents.PositionComponent

  # System behaviour

  @impl true
  def run(%EntitySpawned{entity: entity, components: components}, _delta) do
    components
    |> Enum.find(&match?(%PositionComponent{}, &1))
    |> then(&broadcast_event(:entity_spawn, entity, components, &1))
  end

  def run(%EntityDespawn{entity: entity, components: components}, _delta) do
    components
    |> Enum.find(&match?(%PositionComponent{}, &1))
    |> then(&broadcast_event(:entity_despawn, entity, components, &1))
  end

  ## Helpers

  defp broadcast_event(event, entity, components, %PositionComponent{map_ref: map_ref}) do
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
    GameService.broadcast_to({event, bundle}, endpoints)
  end
end
