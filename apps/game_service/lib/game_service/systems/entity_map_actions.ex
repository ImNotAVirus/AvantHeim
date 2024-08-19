defmodule GameService.EntityMapActionsSystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityMapActionsSystem
  """

  use GameService.System,
    lock_components: [
      GameService.EntityComponents.PositionComponent,
      GameService.EntityComponents.DirectionComponent,
      GameService.EntityComponents.LevelComponent,
      GameService.PlayerComponents.HeroLevelComponent,
      GameService.EntityComponents.CombatComponent,
      # GameService.EntityComponents.BuffComponent,
      GameService.EntityComponents.SpeedComponent,
      GameService.EntityComponents.SittingComponent
    ],
    event_subscriptions: [
      GameService.Events.EntityChangeDirection,
      GameService.Events.EntityInfoRequest,
      GameService.Events.EntityMove,
      GameService.Events.EntitySit,
      GameService.Events.UsePortalRequest
    ]

  require Logger

  alias GameService.Structures.PortalStructure
  alias GameService.GameConfig

  alias GameService.Events.{
    EntityChangeDirection,
    # EntityInfoRequest,
    EntityMapChange,
    EntityMove,
    # EntitySit,
    UsePortalRequest
  }

  # System behaviour

  @impl true
  def run(%EntityChangeDirection{} = event, _context) do
    %EntityChangeDirection{
      entity_type: entity_type,
      entity_id: entity_id,
      value: value
    } = event

    # In the GameService, Entity's id is a combination of it's type and it's id 
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         # Then update it's DirectionComponent
         {:ok, _} <- Command.update_component(entity, E.DirectionComponent, value: value),
         # Then get the current map
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent) do
      # Finally, notify all players on map
      event = {:direction_changed, entity_type, entity_id, value}

      # Here, the 3rd component means that we don't want to send the event to ourself
      _ = GameService.System.map_event(event, position, [entity])

      :ok
    end
    |> maybe_print_error(event)
  end

  @impl true
  def run(%EntityMove{} = event, _context) do
    %EntityMove{
      entity_type: entity_type,
      entity_id: entity_id,
      pos_x: pos_x,
      pos_y: pos_y,
      speed: speed,
      checksum: checksum
    } = event

    # In the GameService, Entity's id is a combination of it's type and it's id 
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         # Validate checksum
         :ok <- validate_checksum(pos_x, pos_y, checksum),
         # Validate speed
         :ok <- validate_speed(entity, speed),
         # TODO: Later we also need to check is the deplacement is allowed (no collision, not too far, ...)
         {:ok, _} <-
           Command.update_component(entity, E.PositionComponent, map_x: pos_x, map_y: pos_y),
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent) do
      # Finally, notify all players on map
      event = {:entity_move, entity_type, entity_id, pos_x, pos_y, speed}

      # Here, the 3rd component means that we don't want to send the event to ourself
      _ = GameService.System.map_event(event, position, [entity])

      :ok
    end
    |> maybe_print_error(event)
  end

  @impl true
  def run(%UsePortalRequest{player_id: entity_id} = event, _context) do
    entity_type = :player

    # In the GameService, Entity's id is a combination of it's type and it's id 
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         # Then get the current map
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent),
         # Then fetch current portal using player position
         {:ok, %PortalStructure{} = portal} <- find_portal(position) do
      # Then send the map change event
      {:ok, _events} =
        ElvenGard.ECS.push(
          %EntityMapChange{
            entity_type: entity_type,
            entity_id: entity_id,
            destination_map_id: portal.destination_map_id,
            destination_map_ref: portal.destination_map_ref,
            destination_map_x: portal.destination_map_x,
            destination_map_y: portal.destination_map_y
          },
          partition: position.map_ref
        )
    end
    |> maybe_print_error(event)
  end

  @impl true
  def run(event, _context) do
    Logger.warning("#{inspect(__MODULE__)} unhandled event #{inspect(event)}")
  end

  ## Helpers

  defp validate_checksum(pos_x, pos_y, checksum) do
    case rem(rem(pos_x + pos_y, 3), 2) == checksum do
      true -> :ok
      false -> {:error, :bad_checksum}
    end
  end

  defp validate_speed(entity, speed) do
    case Query.fetch_component(entity, E.SpeedComponent) do
      {:ok, %E.SpeedComponent{value: ^speed}} -> :ok
      _ -> {:error, :invalid_speed}
    end
  end

  defp find_portal(%E.PositionComponent{} = position) do
    portals = GameConfig.map_portals(position.map_id)

    fun = fn %PortalStructure{} = portal ->
      position.map_x in (portal.source_map_x - 1)..(portal.source_map_x + 1) and
        position.map_y in (portal.source_map_y - 1)..(portal.source_map_y + 1)
    end

    case Enum.find(portals, fun) do
      nil -> {:error, :portal_not_found}
      portal -> {:ok, portal}
    end
  end

  defp maybe_print_error(:ok, _), do: :ok
  defp maybe_print_error({:ok, _}, _), do: :ok

  defp maybe_print_error({:error, _} = error, event) do
    System.error(__MODULE__, error, event)
  end
end
