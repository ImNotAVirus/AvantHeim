defmodule GameService.EntityMessageSystemTest do
  use GameService.SystemCase

  import ExUnit.CaptureLog

  alias GameService.EntityMessageSystem

  ## Tests

  describe "EntityMessage" do
    test "system event on entity writting in general chat" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      direction = %E.DirectionComponent{value: :south}
      entity = spawn_player(components: [endpoint, position, direction])

      # Call our System with a EntityMessage event
      event = %Evt.EntityMessage{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        scope: :map,
        message: "Best message"
      }

      _ = EntityMessageSystem.run(event, 0)

      # # We shouldn't receive an event
      refute_received {:chat_message, _, _, _}
    end
  end
end
