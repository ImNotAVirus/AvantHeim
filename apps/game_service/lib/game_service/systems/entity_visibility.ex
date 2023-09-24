defmodule GameService.EntityVisibilitySystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityVisibilitySystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use GameService.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.EntityMapEnter,
      GameService.Events.EntityDespawned
    ]

  alias GameService.Events.{EntityMapEnter, EntityDespawned}

  # System behaviour

  @impl true
  def run(%EntityMapEnter{entity_type: entity_type, entity_id: entity_id}, _delta) do
    # Get Entity PositionComponent
    ecs_id = GameService.real_entity_id(entity_type, entity_id)
    {:ok, entity} = Query.fetch_entity(ecs_id)
    {:ok, position} = Query.fetch_component(entity, E.PositionComponent)

    # Get all Entities with all Components on the map
    entities =
      ElvenGard.ECS.Entity
      |> Query.select(
        with: [{E.PositionComponent, [{:==, :map_ref, position.map_ref}]}],
        preload: :all
      )
      |> Query.all()

    # Send Events
    Enum.each(entities, fn {entity, components} ->
      _ = broadcast_event(:entity_map_enter, entity, components, position)
    end)
  end

  def run(%EntityDespawned{entity: entity, components: components}, _delta) do
    components
    |> Enum.find(&match?(%E.PositionComponent{}, &1))
    |> then(&broadcast_event2(:entity_despawn, entity, components, &1))
  end

  ## Helpers

  defp broadcast_event(event_name, entity, components, %E.PositionComponent{} = position) do
    # Transform the entity + components to a bundle
    bundle = GameService.load_bundle(entity, components)

    # Send Events
    GameService.System.map_event({event_name, bundle}, position)
  end

  # FIXME: Remove the EntityDespawned
  defp broadcast_event2(event_name, entity, components, %E.PositionComponent{} = position) do
    # Transform the entity + components to a bundle
    bundle = GameService.preload_bundle(entity, components)

    # Send Events
    GameService.System.map_event({event_name, bundle}, position)
  end
end
