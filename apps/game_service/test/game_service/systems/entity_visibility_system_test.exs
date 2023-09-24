defmodule GameService.EntityVisibilitySystemTest do
  use GameService.SystemCase, async: true

  alias GameService.PlayerBundle
  alias GameService.EntityVisibilitySystem

  ## Tests

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
      _ = EntityVisibilitySystem.run(event, 0)

      # We should receive an event with a bundle
      assert_receive {:entity_map_enter, %PlayerBundle{id: ^id} = bundle}
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
      _ = EntityVisibilitySystem.run(event, 0)

      # We should receive events with our old Entities
      assert_receive {:entity_map_enter, %PlayerBundle{id: ^id}}
      assert_receive {:entity_map_enter, %PlayerBundle{id: ^id1}}
      assert_receive {:entity_map_enter, %PlayerBundle{id: ^id2}}
    end
  end

  test "system notify on Entity despawn" do
    # Register our process to receive message
    ref = make_ref()
    position = %E.PositionComponent{map_ref: ref}
    endpoint = %P.EndpointComponent{pid: self()}
    _ = spawn_player(components: [endpoint, position])

    # Call our System with a EntityDespawned event
    entity = spawn_player(components: [position])
    event = %Evt.EntityDespawned{entity: entity, components: [position]}
    _ = EntityVisibilitySystem.run(event, 0)

    # We should receive an event
    assert_receive {:entity_despawn, bundle}
    assert %PlayerBundle{} = bundle
    assert bundle.position == position
  end
end
