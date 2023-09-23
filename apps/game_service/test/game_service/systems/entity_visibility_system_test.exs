defmodule GameService.EntityVisibilitySystemTest do
  use GameService.SystemCase, async: true

  alias GameService.PlayerBundle
  alias GameService.EntityVisibilitySystem

  ## Tests

  test "system notify on Entity spawn" do
    # Register our process to receive message
    ref = make_ref()
    position = %E.PositionComponent{map_ref: ref}
    endpoint = %P.EndpointComponent{pid: self()}
    _ = spawn_player(components: [endpoint, position])

    # Call our System with a EntitySpawned event
    entity = spawn_player(components: [position])
    event = %Evt.EntitySpawned{entity: entity, components: [position]}
    _ = EntityVisibilitySystem.run(event, 0)

    # We should receive an event
    assert_receive {:entity_spawn, bundle}
    assert %PlayerBundle{} = bundle
    assert bundle.position == position
  end

  test "system notify on Entity despawn" do
    # Register our process to receive message
    ref = make_ref()
    position = %E.PositionComponent{map_ref: ref}
    endpoint = %P.EndpointComponent{pid: self()}
    _ = spawn_player(components: [endpoint, position])

    # Call our System with a EntitySpawned event
    entity = spawn_player(components: [position])
    event = %Evt.EntityDespawned{entity: entity, components: [position]}
    _ = EntityVisibilitySystem.run(event, 0)

    # We should receive an event
    assert_receive {:entity_despawn, bundle}
    assert %PlayerBundle{} = bundle
    assert bundle.position == position
  end
end
