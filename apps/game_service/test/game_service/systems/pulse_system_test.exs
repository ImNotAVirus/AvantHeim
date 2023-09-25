defmodule GameService.PulseSystemTest do
  use GameService.SystemCase

  import ExUnit.CaptureLog

  alias GameService.PulseSystem

  ## Tests

  describe "System" do
    test "automatically notify on invalid pulse" do
      # Register our process to receive message
      endpoint = %P.EndpointComponent{pid: self()}
      pulse = %P.PulseComponent{last_time: last_time(), value: 0}
      entity = spawn_player(components: [endpoint, pulse])

      # Call our System
      _ = PulseSystem.run(0)

      # Nothing happened, the PulseComponent is valid
      refute_received {:invalid_pulse, _}

      # Now break the PulseComponent: last_time
      _ = Command.update_component(entity, P.PulseComponent, last_time: last_time() - 6_000)

      # Call our System
      PulseSystem.run(0)

      # We should received a message
      assert_received {:invalid_pulse, :expired}
    end
  end

  describe "PlayerPulse event" do
    test "update the PulseComponent" do
      # Create a dummy Entity with a PulseComponent
      pulse = %P.PulseComponent{last_time: last_time(), value: 60}
      entity = spawn_player(components: [pulse])
      type = GameService.entity_type(entity)
      id = GameService.entity_id(entity)

      # Call our System with a PlayerPulse event
      event = %Evt.PlayerPulse{entity_type: type, entity_id: id, value: 120, inserted_at: now()}
      _ = PulseSystem.run(event, 0)

      # PlayerPulse should have been modified
      {:ok, component} = Query.fetch_component(entity, P.PulseComponent)
      assert component.value == 120
      assert component.last_time > pulse.last_time
    end

    test "should notify on invalid pulse" do
      # Register our process to receive message
      endpoint = %P.EndpointComponent{pid: self()}
      pulse = %P.PulseComponent{last_time: last_time(), value: 60}
      entity = spawn_player(components: [endpoint, pulse])
      type = GameService.entity_type(entity)
      id = GameService.entity_id(entity)

      # Call our System with a PlayerPulse event
      event = %Evt.PlayerPulse{entity_type: type, entity_id: id, value: 120, inserted_at: now()}
      _ = PulseSystem.run(event, 0)

      # Nothing happened, the PulseComponent is valid
      refute_received {:invalid_pulse, _}

      # Now break the PulseComponent: last_time
      _ = Command.update_component(entity, P.PulseComponent, last_time: last_time() - 6_000)
      event = %Evt.PlayerPulse{entity_type: type, entity_id: id, value: 180, inserted_at: now()}

      # We should have an error message
      fun = fn -> _ = PulseSystem.run(event, 0) end
      assert capture_log(fun) =~ "failed with value {:invalid_pulse, :time}"

      # We should received a message
      assert_received {:invalid_pulse, :time}

      # Now break the PulseComponent: value
      _ = Command.update_component(entity, P.PulseComponent, last_time: last_time())
      event = %Evt.PlayerPulse{entity_type: type, entity_id: id, value: 0, inserted_at: now()}

      # We should have an error message
      fun = fn -> _ = PulseSystem.run(event, 0) end
      assert capture_log(fun) =~ "failed with value {:invalid_pulse, :value}"

      # We should received a message
      assert_received {:invalid_pulse, :value}
    end
  end

  ## Helpers

  defp now(), do: ElvenGard.ECS.now()
  defp last_time(), do: ElvenGard.ECS.now() - :timer.seconds(60)
end
