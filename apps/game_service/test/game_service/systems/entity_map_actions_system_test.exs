defmodule GameService.EntityMapActionsSystemTest do
  use GameService.SystemCase, async: true

  alias GameService.EntityMapActionsSystem

  ## Tests

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
    assert_receive {:direction_changed, entity_type, entity_id, value}
    assert entity_type == event.entity_type
    assert entity_id == event.entity_id
    assert value == event.value
  end
end
