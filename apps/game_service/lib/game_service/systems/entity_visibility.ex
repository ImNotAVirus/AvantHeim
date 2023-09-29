defmodule GameService.EntityVisibilitySystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityVisibilitySystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use GameService.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.EntityMapEnter,
      GameService.Events.EntityMapLeave
    ]

  alias GameService.Events.{EntityMapEnter, EntityMapLeave}

  # System behaviour

  @impl true
  def run(%EntityMapEnter{entity_type: entity_type, entity_id: entity_id}, _delta) do
    # Get Entity PositionComponent
    ecs_id = GameService.real_entity_id(entity_type, entity_id)
    {:ok, entity} = Query.fetch_entity(ecs_id)
    {:ok, position} = Query.fetch_component(entity, E.PositionComponent)

    # Create the bundle
    {:ok, components} = ElvenGard.ECS.Query.list_components(entity)
    bundle = GameService.load_bundle(entity, components)

    # Notify all Endpoint on the same map except ourself
    GameService.System.map_event({:entity_map_enter, bundle}, position, [entity])

    # If the Entity has an EndpointComponent, notify the map change and
    # send him all entities on the map
    with {:ok, endpoint} <- Query.fetch_component(entity, P.EndpointComponent) do
      _ = GameService.send_to({:map_change, bundle}, endpoint)

      position
      |> list_map_bundles()
      |> Enum.reject(&(&1.id == entity_id))
      |> Enum.map(&{:entity_map_enter, &1})
      |> GameService.send_to(endpoint)
    end
  end

  def run(%EntityMapLeave{entity_type: entity_type, entity_id: entity_id}, _delta) do
    # Get Entity PositionComponent
    ecs_id = GameService.real_entity_id(entity_type, entity_id)
    {:ok, entity} = Query.fetch_entity(ecs_id)
    {:ok, position} = Query.fetch_component(entity, E.PositionComponent)

    # If the Entity has an EndpointComponent, notify the map leave
    with {:ok, endpoint} <- Query.fetch_component(entity, P.EndpointComponent) do
      _ = GameService.send_to(:map_leave, endpoint)
    end

    # Notify all Endpoint on the same map except ourself
    event = {:entity_map_leave, entity_type, entity_id}
    GameService.System.map_event(event, position, [entity])
  end

  ## Helpers

  defp list_map_bundles(position) do
    # Get all Entities with all Components on the map
    ElvenGard.ECS.Entity
    |> Query.select(
      with: [{E.PositionComponent, [{:==, :map_ref, position.map_ref}]}],
      preload: :all
    )
    |> Query.all()
    |> Enum.map(&GameService.load_bundle(elem(&1, 0), elem(&1, 1)))
  end
end
