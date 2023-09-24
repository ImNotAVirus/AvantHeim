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
      GameService.Events.EntitySit
    ]

  require Logger

  alias GameService.Events.{
    EntityChangeDirection,
    # EntityInfoRequest,
    EntityMove
    # EntitySit
  }

  # System behaviour

  @impl true
  def run(%EntityChangeDirection{} = event, _delta) do
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
      GameService.System.map_event(event, position, [entity])
    end
    |> maybe_print_error(event)
  end

  def run(%EntityMove{} = event, _delta) do
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
      GameService.System.map_event(event, position, [entity])
    end
    |> maybe_print_error(event)
  end

  def run(event, _delta) do
    Logger.warn("#{inspect(__MODULE__)} unhandled event #{inspect(event)}")
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

  defp maybe_print_error({:error, _} = error, event) do
    Logger.error(
      "[#{inspect(__MODULE__)}] #{inspect(event.__struct__)} event failed " <>
        "with value #{inspect(error)} - #{inspect(event, limit: :infinity)}"
    )
  end

  defp maybe_print_error(_, _), do: :ok
end
