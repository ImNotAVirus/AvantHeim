defmodule GameService.EntityMessageSystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityMessageSystem
  """

  use GameService.System,
    lock_components: [],
    event_subscriptions: [
      GameService.Events.EntityMessage
    ]

  require Logger

  alias GameService.Events.EntityMessage

  # System behaviour

  @impl true
  def run(%EntityMessage{scope: :map} = event, _context) do
    # Creating our event message, to send map message
    %EntityMessage{
      entity_type: entity_type,
      entity_id: entity_id,
      message: message
    } = event

    # In the GameService, Entity's id is a combination of it's type and it's id
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent) do
      # Finally, notify all players on map
      event = {:chat_message, entity_type, entity_id, message}

      # Here, the 3rd component means that we don't want to send the event to ourself
      GameService.System.map_event(event, position, [entity])
    end
  end

  @impl true
  def run(%EntityMessage{scope: :private, target_type: :player} = event, _context) do
    # Creating our event message, to send map message
    %EntityMessage{
      entity_type: entity_type,
      entity_id: entity_id,
      message: message,
      target_id: target_id
    } = event

    # In the GameService, Entity's id is a combination of it's type and it's id
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    endpoints =
      P.EndpointComponent
      |> Query.select(with: [{P.PlayerComponent, [{:==, :id, target_id}]}])
      |> Query.all()

    # Check if the Entity exists
    with :ok <- Query.fetch_entity(ecs_id),
         :ok do
      # Finally, notify player with target_id
      event = {:chat_message, entity_type, entity_id, message}

      GameService.broadcast_to(event, endpoints)
    end
  end
end
