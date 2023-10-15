defmodule GameService.EntityMessageSystemTest do
  use GameService.SystemCase

  import ExUnit.CaptureLog

  alias GameService.EntityMessageSystem

  ## Tests

  describe "EntityMessage" do
    test "system souldn't send back our messages" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      entity = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMessage event
      event = %Evt.EntityMessage{
        entity_type: GameService.entity_type(entity),
        entity_id: GameService.entity_id(entity),
        scope: :map,
        message: "Best message"
      }

      _ = EntityMessageSystem.run(event, 0)

      # # We shouldn't receive an event
      refute_received {:entity_message, _, _, _}
    end

    test "system notify on entity map writting" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity
      entity = spawn_player(components: [position])

      # Call our System with a EntityMessage event
      event = %Evt.EntityMessage{
        entity_type: GameService.entity_type(entity),
        entity_id: GameService.entity_id(entity),
        scope: :map,
        message: "Best message from other player"
      }

      _ = EntityMessageSystem.run(event, 0)

      # # We should receive an event
      assert_received {:entity_message, entity_type, entity_id, message}
      assert entity_type == event.entity_type
      assert entity_id == event.entity_id
      assert message == event.message
    end

    test "system shouldn't send back our event for private messages" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      entity = spawn_player(components: [endpoint, position])
      target_entity = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMessage event
      event = %Evt.EntityMessage{
        entity_type: GameService.entity_type(entity),
        entity_id: GameService.entity_id(entity),
        scope: :private,
        message: "Best private message",
        player_name: "DarkyZ"
      }

      _ = EntityMessageSystem.run(event, 0)

      # # We shouldn't receive an event
      refute_received {:whisper, _, _, _}
    end

    test "system notify on entity private writting" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity
      entity = spawn_player(components: [position])

      # Call our System with a EntityMessage event
      event = %Evt.EntityMessage{
        scope: :private,
        player_name: "PlayerName",
        message: "Best private message from other player"
      }

      _ = EntityMessageSystem.run(event, 0)

      # # We should receive an event
      assert_received {:private_message, player_name, message}
      assert player_name == event.player_name
      assert message == event.message
    end
  end
end
