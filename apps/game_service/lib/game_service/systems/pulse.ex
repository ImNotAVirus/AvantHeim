defmodule GameService.PulseSystem do
  @moduledoc """
  TODO: Documentation for GameService.PulseSystem

  TL;DR; Update the PulseComponent for a Player and send a disconnection 
  event to everyone who doesn't send his pulse
  """

  use GameService.System,
    lock_components: [GameService.PlayerComponents.PulseComponent],
    event_subscriptions: [
      GameService.Events.PlayerPulse
    ]

  require Logger

  alias GameService.Events.PlayerPulse

  @inc 60
  @delta :timer.seconds(5)

  # System behaviour

  @impl true
  def run(%PlayerPulse{} = event, _delta) do
    %PlayerPulse{
      entity_type: entity_type,
      entity_id: entity_id,
      value: value,
      inserted_at: inserted_at
    } = event

    # Get Entity
    ecs_id = GameService.real_entity_id(entity_type, entity_id)
    {:ok, entity} = Query.fetch_entity(ecs_id)

    # Get the prev pulse
    with {:ok, old_pulse} <- Query.fetch_component(entity, P.PulseComponent),
         # Validate the pulse
         :ok <- validate_value(old_pulse, value),
         :ok <- validate_time(old_pulse, inserted_at) do
      # Then update the PulseComponent
      {:ok, _} =
        ElvenGard.ECS.Command.update_component(entity, P.PulseComponent,
          last_time: inserted_at,
          value: value
        )
    else
      {:error, {:invalid_pulse, _} = error} ->
        # Print the error
        _ = System.error(__MODULE__, error, event)

        # If Pulse event is invalid, we must notify the Endpoint
        {:ok, endpoint} = Query.fetch_component(entity, P.EndpointComponent)
        GameService.send_to(error, endpoint)

      _ ->
        # This case match if the PulseComponent is not found so it shouldn't happen
        :ignore
    end
  end

  ## Helpers

  def validate_value(old_pulse, value) do
    case value == old_pulse.value + @inc do
      true -> :ok
      false -> {:error, {:invalid_pulse, :value}}
    end
  end

  def validate_time(old_pulse, inserted_at) do
    new_time = old_pulse.last_time + :timer.seconds(@inc)
    min_time = new_time - @delta
    max_time = new_time + @delta

    case {inserted_at < min_time, inserted_at > max_time} do
      {false, false} -> :ok
      _ -> {:error, {:invalid_pulse, :time}}
    end
  end
end
