defmodule GameService.EntityVisibilitySystemTest do
  use GameService.SystemCase, async: true

  alias GameService.PlayerBundle
  alias GameService.EntityVisibilitySystem
  alias GameService.Events.EntityMapEnter

  ## Tests

  describe "EntityMapChange" do
    test "send an EntityMapEnter event" do
      # Register our process to receive message
      source_map_ref = make_ref()
      destination_map_ref = make_ref()
      position = %E.PositionComponent{map_id: 1, map_ref: source_map_ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapChange event
      %Entity{id: {type, id}} = spawn_player(components: [position])

      event = %Evt.EntityMapChange{
        entity_type: type,
        entity_id: id,
        destination_map_id: 2,
        destination_map_ref: destination_map_ref,
        destination_map_x: 140,
        destination_map_y: 148
      }

      assert {:ok, result} = EntityVisibilitySystem.run(event, 0)
      # FIXME: instead of pattern maching on result,
      # write a mock for a partition and forward all received events
      assert [%EntityMapEnter{}] = result
    end

    test "update PositionComponent" do
      # Register our process to receive message
      source_map_ref = make_ref()
      destination_map_ref = make_ref()
      position = %E.PositionComponent{map_id: 1, map_ref: source_map_ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapChange event
      %Entity{id: {type, id}} = entity = spawn_player(components: [position])

      event = %Evt.EntityMapChange{
        entity_type: type,
        entity_id: id,
        destination_map_id: 2,
        destination_map_ref: destination_map_ref,
        destination_map_x: 140,
        destination_map_y: 148
      }

      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # Check that the PositionComponent was updated
      {:ok, component} = Query.fetch_component(entity, E.PositionComponent)

      assert component == %E.PositionComponent{
               map_id: 2,
               map_ref: destination_map_ref,
               map_x: 140,
               map_y: 148
             }
    end

    test "send an entity_map_leave event" do
      # Register our process to receive message
      source_map_ref = make_ref()
      destination_map_ref = make_ref()
      position = %E.PositionComponent{map_id: 1, map_ref: source_map_ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapChange event
      %Entity{id: {type, id}} = spawn_player(components: [position])

      event = %Evt.EntityMapChange{
        entity_type: type,
        entity_id: id,
        destination_map_id: 2,
        destination_map_ref: destination_map_ref,
        destination_map_x: 140,
        destination_map_y: 148
      }

      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive an event with a bundle
      assert_received {:entity_map_leave, ^type, ^id}
    end
  end

  describe "EntityMapEnter" do
    test "system notify on Entity spawn" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapEnter event
      %Entity{id: {type, id}} = spawn_player(components: [position])
      event = %Evt.EntityMapEnter{entity_type: type, entity_id: id}
      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive an event with a bundle
      assert_received {:entity_map_enter, %PlayerBundle{id: ^id} = bundle}
      assert bundle.position == position
    end

    test "system notify on map change" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      %Entity{id: {type, id}} = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapEnter event
      event = %Evt.EntityMapEnter{entity_type: type, entity_id: id}
      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive an event with a bundle
      assert_received {:map_change, %PlayerBundle{id: ^id} = bundle}
      assert bundle.position == position
    end

    test "send to the new Entity others that are on the map" do
      # Register some dummies
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      %Entity{id: {_, id1}} = spawn_player(components: [position])
      %Entity{id: {_, id2}} = spawn_player(components: [position])

      # Register our process to receive message
      endpoint = %P.EndpointComponent{pid: self()}
      %Entity{id: {type, id}} = spawn_player(components: [endpoint, position])
      event = %Evt.EntityMapEnter{entity_type: type, entity_id: id}
      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive events with our old Entities
      refute_received {:entity_map_enter, %PlayerBundle{id: ^id}}
      assert_received {:entity_map_enter, %PlayerBundle{id: ^id1}}
      assert_received {:entity_map_enter, %PlayerBundle{id: ^id2}}
    end
  end

  describe "EntityMapLeave" do
    test "system notify other on Entity leave" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      _ = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapLeave event
      %Entity{id: {type, id}} = spawn_player(components: [position])
      event = %Evt.EntityMapLeave{entity_type: type, entity_id: id}
      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive an event
      assert_received {:entity_map_leave, ^type, ^id}
    end

    test "system notify ourself on Entity leave" do
      # Register our process to receive message
      ref = make_ref()
      position = %E.PositionComponent{map_ref: ref}
      endpoint = %P.EndpointComponent{pid: self()}
      %Entity{id: {type, id}} = spawn_player(components: [endpoint, position])

      # Call our System with a EntityMapLeave event
      event = %Evt.EntityMapLeave{entity_type: type, entity_id: id}
      assert {:ok, _} = EntityVisibilitySystem.run(event, 0)

      # We should receive an event
      assert_received :map_leave
    end
  end
end
