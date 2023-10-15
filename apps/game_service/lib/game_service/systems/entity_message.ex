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
    %EntityMessage{
      entity_type: entity_type,
      entity_id: entity_id,
      message: message
    } = event

    message = String.slice(message, 0, 60)

    # In the GameService, Entity's id is a combination of it's type and it's id
    ecs_id = GameService.real_entity_id(entity_type, entity_id)

    # Check if the Entity exists
    with {:ok, entity} <- Query.fetch_entity(ecs_id),
         {:ok, position} <- Query.fetch_component(entity, E.PositionComponent) do
      # Finally, notify all players on map
      event = {:entity_message, entity_type, entity_id, message}

      # Here, the 3rd component means that we don't want to send the event to ourself
      GameService.System.map_event(event, position, [entity])
    end
  end

  @impl true
  def run(%EntityMessage{scope: :private} = event, _context) do
    %EntityMessage{
      player_name: player_name,
      message: message
    } = event

    message = String.slice(message, 0, 60)

    endpoint =
      P.EndpointComponent
      |> Query.select(with: [{P.PlayerComponent, [{:==, :name, player_name}]}])
      |> Query.one()

    event = {:private_message, player_name, message}

    case endpoint do
      nil -> Logger.warn("A player tried to whisper to a not connected character")
      _ -> GameService.send_to(event, endpoint)
    end
  end
end
