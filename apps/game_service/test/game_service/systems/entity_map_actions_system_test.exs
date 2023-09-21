defmodule GameService.EntityMapActionsSystemTest do
  use GameService.EntityCase, async: true

  alias ElvenGard.ECS.Query

  alias GameService.Events, as: Evt
  alias GameService.EntityMapActionsSystem
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

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

    # Call our System with a DirectionChanged event
    event = %Evt.DirectionChanged{
      entity_type: :character,
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
