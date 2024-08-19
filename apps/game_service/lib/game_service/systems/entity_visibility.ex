defmodule GameService.EntityVisibilitySystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityVisibilitySystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use GameService.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.EntityMapChange,
      GameService.Events.EntityMapEnter,
      GameService.Events.EntityMapLeave
    ]

  alias GameService.Events.{EntityMapChange, EntityMapEnter, EntityMapLeave}
  alias GameService.GameConfig

  # System behaviour

  @impl true
  def run(%EntityMapChange{} = event, context) do
    %EntityMapChange{
      entity_type: entity_type,
      entity_id: entity_id,
      destination_map_id: destination_map_id,
      destination_map_ref: destination_map_ref,
      destination_map_x: destination_map_x,
      destination_map_y: destination_map_y
    } = event

    # First execute the entity leave event
    _ =
      run(
        %EntityMapLeave{
          entity_type: entity_type,
          entity_id: entity_id,
          # FIXME: Don't create the struct manually, use something like
          # ElvenGard.ECS.Event.new(EntityMapLeave, %{ attrs...})
          inserted_at: event.inserted_at
        },
        context
      )

    # Then update the PositionComponent
    ecs_id = GameService.real_entity_id(entity_type, entity_id)
    {:ok, entity} = Query.fetch_entity(ecs_id)

    {:ok, new_position} =
      Command.update_component(entity, E.PositionComponent,
        map_id: destination_map_id,
        map_ref: destination_map_ref,
        map_x: destination_map_x,
        map_y: destination_map_y
      )

    # Finally push the map enter event to the new partition
    {:ok, _events} =
      ElvenGard.ECS.push(
        %EntityMapEnter{entity_type: entity_type, entity_id: entity_id},
        partition: new_position.map_ref
      )
  end

  @impl true
  def run(%EntityMapEnter{entity_type: entity_type, entity_id: entity_id}, _context) do
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
      # Send map change event
      _ = GameService.send_to({:map_change, bundle}, endpoint)

      # Send all entities bundles
      position
      |> list_map_bundles()
      |> Enum.reject(&(&1.id == entity_id))
      |> Enum.map(&{:entity_map_enter, &1})
      |> GameService.send_to(endpoint)

      # Send portals
      # FIXME: only works for static map, not instances
      portals = GameConfig.map_portals(position.map_id)
      _events = GameService.send_to({:show_portals, portals}, endpoint)
    end

    {:ok, {}}
  end

  @impl true
  def run(%EntityMapLeave{entity_type: entity_type, entity_id: entity_id}, _context) do
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
    _events = GameService.System.map_event(event, position, [entity])

    {:ok, {}}
  end

  ## Helpers

  defp list_map_bundles(position) do
    # Get all Entities with all Components on the map
    ElvenGard.ECS.Entity
    |> Query.select(
      preload: :all,
      partition: position.map_ref
    )
    |> Query.all()
    |> Enum.map(&GameService.load_bundle(elem(&1, 0), elem(&1, 1)))
  end
end
