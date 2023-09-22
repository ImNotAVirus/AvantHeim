defmodule GameService.EntityVisibilitySystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityVisibilitySystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use ElvenGard.ECS.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.EntitySpawned,
      GameService.Events.EntityDespawned
    ]

  alias GameService.Events.{EntitySpawned, EntityDespawned}
  alias GameService.EntityComponents, as: E

  # System behaviour

  @impl true
  def run(%EntitySpawned{entity: entity, components: components}, _delta) do
    components
    |> Enum.find(&match?(%E.PositionComponent{}, &1))
    |> then(&broadcast_event(:entity_spawn, entity, components, &1))
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
