defmodule GameService.SessionDisconnectionSystem do
  @moduledoc """
  TODO: Documentation for GameService.SessionDisconnectionSystem

  This System is responsible of cleaning the System and notify all Endpoints
  in case of player disconnection
  """

  use GameService.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.PlayerDisconnected
    ]

  require Logger

  alias GameService.Events.PlayerDisconnected

  # System behaviour

  @impl true
  def run(%PlayerDisconnected{account_id: account_id}, _delta) do
    # Get the disconnected Entity and his PositionComponent
    {entity, position} =
      {ElvenGard.ECS.Entity, E.PositionComponent}
      |> Query.select(with: [{P.AccountComponent, [{:==, :id, account_id}]}])
      |> Query.one()

    # Remove the Entity from our systems
    {:ok, _tuple} = Command.despawn_entity(entity)

    # Notify all Endpoint on the map except ourself
    event = {:entity_map_leave, :player, GameService.entity_id(entity)}
    GameService.System.map_event(event, position, [entity])

    # TODO: Later, also remove from group, update friendlist, ...
  end
end
