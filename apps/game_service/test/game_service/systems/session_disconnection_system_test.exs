defmodule GameService.SessionDisconnectionSystemTest do
  use GameService.SystemCase, async: true

  alias GameService.SessionDisconnectionSystem

  ## Tests

  describe "PlayerDisconnected" do
    test "system notify on Player disconnect" do
      # Register our process to receive message
      map_ref = make_ref()
      position = %E.PositionComponent{map_ref: map_ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity with a AccountComponent
      account_ref = make_ref()
      account = %P.AccountComponent{id: account_ref}
      %Entity{id: {type, id}} = spawn_player(components: [account, endpoint, position])

      # Call our System with a PlayerDisconnected event
      event = %Evt.PlayerDisconnected{account_id: account_ref}
      _ = SessionDisconnectionSystem.run(event, 0)

      # We should receive an event
      assert_receive {:entity_map_leave, ^type, ^id}
    end

    test "remove Player from engine" do
      # Create our fake Entity with a AccountComponent
      map_ref = make_ref()
      account_ref = make_ref()
      account = %P.AccountComponent{id: account_ref}
      position = %E.PositionComponent{map_ref: map_ref}
      %Entity{id: id} = spawn_player(components: [account, position])

      # Call our System with a PlayerDisconnected event
      event = %Evt.PlayerDisconnected{account_id: account_ref}
      _ = SessionDisconnectionSystem.run(event, 0)

      # Player should be removed
      assert {:error, :not_found} = Query.fetch_entity(id)
    end
  end
end
