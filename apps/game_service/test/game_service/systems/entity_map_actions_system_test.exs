defmodule GameService.EntityMapActionsSystemTest do
  use GameService.SystemCase, async: true

  import ExUnit.CaptureLog

  alias GameService.EntityMapActionsSystem
  alias GameService.Events.EntityMapChange
  alias GameService.GameConfig

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

  describe "UsePortalRequest" do
    test "send an EntityMapChange event" do
      # Get the portal from map 1 to map 2
      source_map_id = 1
      destination_map_id = 2

      portal =
        source_map_id
        |> GameConfig.map_portals()
        |> Enum.find(&(&1.destination_map_id == destination_map_id))

      # Register our process to receive message
      ref = make_ref()

      position = %E.PositionComponent{
        map_id: source_map_id,
        map_ref: ref,
        map_x: portal.source_map_x,
        map_y: portal.source_map_y
      }

      endpoint = %P.EndpointComponent{pid: self()}
      entity = spawn_player(components: [endpoint, position])

      # Call our System with a UsePortalRequest event
      event = %Evt.UsePortalRequest{player_id: GameService.entity_id(entity)}
      assert {:ok, result} = EntityMapActionsSystem.run(event, 0)
      # FIXME: instead of pattern maching on result,
      # write a mock for a partition and forward all received events
      assert [%EntityMapChange{}] = result
    end

    test "do nothing if not on a portal" do
      # Register our process to receive message
      ref = make_ref()

      position = %E.PositionComponent{
        map_id: 1,
        map_ref: ref,
        map_x: 0,
        map_y: 0
      }

      endpoint = %P.EndpointComponent{pid: self()}
      entity = spawn_player(components: [endpoint, position])

      # Call our System with a UsePortalRequest event
      event = %Evt.UsePortalRequest{player_id: GameService.entity_id(entity)}

      fun = fn ->
        _ = EntityMapActionsSystem.run(event, 0)
      end

      # We should have an error message
      assert capture_log(fun) =~ "failed with value {:error, :portal_not_found}"
    end
  end
end
