defmodule GameService.EntityMapActionsSystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityMapActionsSystem
  """

  use ElvenGard.ECS.System,
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
      GameService.Events.ChangeDirection,
      GameService.Events.EntityInfoRequest,
      GameService.Events.EntityMove,
      GameService.Events.EntitySit
    ]

  require Logger

  alias ElvenGard.ECS.{Command, Query}

  alias GameService.EntityComponents, as: E

  alias GameService.Events.{
    ChangeDirection
    # EntityInfoRequest,
    # Movement,
    # Sitting
  }

  # System behaviour

  @impl true
  def run(%ChangeDirection{} = event, _delta) do
    %ChangeDirection{
      entity_type: entity_type,
      entity_id: entity_id,
      value: value
    } = event

    ecs_type = GameService.entity_type_to_prefix(entity_type)
    ecs_id = {ecs_type, entity_id}

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         # Then update it's DirectionComponent
         {:ok, _} <- Command.update_component(entity, E.DirectionComponent, value: value),
         # Then get the current map
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent) do
      # Finally, notify all players on map
      event = {:direction_changed, entity_type, entity_id, value}
      GameService.System.map_event(event, position)
    else
      e ->
        Logger.error(
          "[#{inspect(__MODULE__)}] Can't set direction, " <>
            "got #{inspect(e)} for #{inspect(event)}"
        )
    end
  end

  def run(event, _delta) do
    Logger.warn("#{inspect(__MODULE__)} unhandled event #{inspect(event)}")
  end
end
