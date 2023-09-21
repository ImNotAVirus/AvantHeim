defmodule GameService.EntityVisibilitySystemTest do
  use GameService.EntityCase, async: true

  alias GameService.PlayerBundle
  alias GameService.Events.{EntitySpawned, EntityDespawned}
  alias GameService.PlayerComponents.EndpointComponent
  alias GameService.EntityComponents.PositionComponent
  alias GameService.EntityVisibilitySystem

  ## Tests

  test "system notify on Entity spawn" do
    # Register our process to receive message
    ref = make_ref()
    position = %PositionComponent{map_ref: ref}
    endpoint = %EndpointComponent{pid: self()}
    _ = spawn_player(components: [endpoint, position])

    # Call our System with a EntitySpawned event
    entity = spawn_player(components: [position])
    event = %EntitySpawned{entity: entity, components: [position]}
    _ = EntityVisibilitySystem.run(event, 0)

    # We should receive an event
    assert_receive {:entity_spawn, bundle}
    assert %PlayerBundle{} = bundle
    assert bundle.position == position
  end

  test "system notify on Entity despawn" do
    # Register our process to receive message
    ref = make_ref()
    position = %PositionComponent{map_ref: ref}
    endpoint = %EndpointComponent{pid: self()}
    _ = spawn_player(components: [endpoint, position])

    # Call our System with a EntitySpawned event
    entity = spawn_player(components: [position])
    event = %EntityDespawned{entity: entity, components: [position]}
    _ = EntityVisibilitySystem.run(event, 0)

    # We should receive an event
    assert_receive {:entity_despawn, bundle}
    assert %PlayerBundle{} = bundle
    assert bundle.position == position
  end
end
