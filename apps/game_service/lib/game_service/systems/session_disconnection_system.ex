defmodule GameService.SessionDisconnectionSystem do
  @moduledoc """
  TODO: Documentation for GameService.SessionDisconnectionSystem

  FIXME: Test this system
  """

  use GameService.System,
    lock_components: :sync,
    event_subscriptions: [
      GameService.Events.PlayerDisconnected
    ]

  require Logger

  alias GameService.Events.{EntityDespawned, PlayerDisconnected}

  # System behaviour

  @impl true
  def run(%PlayerDisconnected{account_id: account_id}, _delta) do
    # Get the disconnected Entity and his PositionComponent
    {entity, components} =
      ElvenGard.ECS.Entity
      |> Query.select(
        with: [{P.AccountComponent, [{:==, :id, account_id}]}],
        preload: [E.PositionComponent]
      )
      |> Query.one()

    # Find the PositionComponent
    %E.PositionComponent{map_ref: map_ref} =
      position = Enum.find(components, &(&1.__struct__ == E.PositionComponent))

    # Send the EntityDespawned event to notify player
    # FIXME: Later rewrite this part:
    #   - EntityDespawned should be renamed EntityLeaveMap
    #   - attrs must be only entity_id and map_ref
    {:ok, _events} =
      ElvenGard.ECS.push(
        # Here we only need the position component for the despawn event
        %EntityDespawned{entity: entity, components: [position]},
        partition: map_ref
      )

    # Remove the Entity from our systems
    {:ok, _tuple} = Command.despawn_entity(entity)
  end
end
