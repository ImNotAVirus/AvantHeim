defmodule GameService.EntityMapActionsSystemTest do
  use GameService.SystemCase, async: true

  import ExUnit.CaptureLog

  alias GameService.EntityMapActionsSystem

  ## Tests

  describe "EntityChangeDirection" do
    test "system notify on Entity change direction" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity with a DirectionComponent
      direction = %E.DirectionComponent{value: :south}
      entity = spawn_player(components: [position, direction])

      # Call our System with a EntityChangeDirection event
      event = %Evt.EntityChangeDirection{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        value: :north
      }

      _ = EntityMapActionsSystem.run(event, 0)

      # Check that the DirectionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.DirectionComponent)
      assert %E.DirectionComponent{value: :north} = component

      # # We should receive an event
      assert_received {:direction_changed, entity_type, entity_id, value}
      assert entity_type == event.entity_type
      assert entity_id == event.entity_id
      assert value == event.value
    end

    test "system souldn't send back our messages" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      direction = %E.DirectionComponent{value: :south}
      entity = spawn_player(components: [endpoint, position, direction])

      # Call our System with a EntityChangeDirection event
      event = %Evt.EntityChangeDirection{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        value: :north
      }

      _ = EntityMapActionsSystem.run(event, 0)

      # Check that the DirectionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.DirectionComponent)
      assert %E.DirectionComponent{value: :north} = component

      # # We shouldn't receive an event
      refute_received {:direction_changed, _, _, _}
    end
  end

  describe "EntityMove" do
    test "system notify on Entity move" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity
      position = %E.PositionComponent{map_ref: ref, map_x: 77, map_y: 124}
      speed = %E.SpeedComponent{value: 20}
      entity = spawn_player(components: [position, speed])

      # Call our System with a EntityMove event
      event = %Evt.EntityMove{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        pos_x: 76,
        pos_y: 122,
        speed: 20,
        checksum: 0
      }

      _ = EntityMapActionsSystem.run(event, 0)

      # Check that the PositionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.PositionComponent)
      assert %E.PositionComponent{map_x: 76, map_y: 122} = component

      # # We should receive an event
      assert_received {:entity_move, entity_type, entity_id, 76, 122, 20}
      assert entity_type == event.entity_type
      assert entity_id == event.entity_id
    end

    test "system check for invalid checksum" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity
      position = %E.PositionComponent{map_ref: ref, map_x: 77, map_y: 124}
      speed = %E.SpeedComponent{value: 20}
      entity = spawn_player(components: [position, speed])

      # Call our System with a EntityMove event
      event = %Evt.EntityMove{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        pos_x: 76,
        pos_y: 122,
        speed: 20,
        checksum: 1
      }

      # We should have an error message
      fun = fn -> _ = EntityMapActionsSystem.run(event, 0) end
      assert capture_log(fun) =~ "failed with value {:error, :bad_checksum}"

      # Check that the PositionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.PositionComponent)
      refute %E.PositionComponent{map_x: 76, map_y: 122} == component

      # # We should receive an event
      refute_received {:entity_move, _, _, _, _, _}
    end

    test "system check for invalid speed" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Create our fake Entity
      position = %E.PositionComponent{map_ref: ref, map_x: 77, map_y: 124}
      speed = %E.SpeedComponent{value: 20}
      entity = spawn_player(components: [position, speed])

      # Call our System with a EntityMove event
      event = %Evt.EntityMove{
        entity_type: :player,
        entity_id: GameService.entity_id(entity),
        pos_x: 76,
        pos_y: 122,
        speed: 25,
        checksum: 0
      }

      fun = fn ->
        _ = EntityMapActionsSystem.run(event, 0)
      end

      # We should have an error message
      assert capture_log(fun) =~ "failed with value {:error, :invalid_speed}"

      # Check that the PositionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.PositionComponent)
      refute %E.PositionComponent{map_x: 76, map_y: 122} == component

      # # We should receive an event
      refute_received {:entity_move, _, _, _, _, _}
    end
  end
end
