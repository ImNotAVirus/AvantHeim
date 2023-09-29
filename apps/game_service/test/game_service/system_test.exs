defmodule GameService.SystemTest do
  use GameService.SystemCase, async: true

  alias GameService.System

  ## Tests

  describe "map_event/3" do
    test "send events to the same map" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Send an event and assert received
      _ = System.map_event({:event, :same_map}, position)
      assert_received {:event, :same_map}
    end

    test "doesn't send event if on the ignored list" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      entity = spawn_player(components: [endpoint, position])

      # Send an event and assert received
      _ = System.map_event({:event, :same_map}, position, [entity])
      refute_received {:event, :same_map}
    end
  end
end
