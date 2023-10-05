defmodule GameService.AIMovementSystem do
  @moduledoc """
  TODO: Documentation for GameService.AIMovementSystem
  """

  use GameService.System,
    lock_components: [
      GameService.MonsterComponents.AIMovementComponent,
      GameService.EntityComponents.PositionComponent,
      GameService.EntityComponents.SpeedComponent
    ]

  require Logger

  # System behaviour

  @impl true
  def run(_context) do
    now = ElvenGard.ECS.now()

    # Get monster that need to move
    {ElvenGard.ECS.Entity, E.PositionComponent, M.AIMovementComponent, E.SpeedComponent}
    # FIXME: Why this doesn't work wtf
    # |> Query.select(with: [{M.AIMovementComponent, [{:>=, :next_move, now}]}])
    |> Query.select(with: [M.AIMovementComponent])
    |> Query.all()
    |> Enum.filter(&(elem(&1, 2).next_move <= now))
    |> Enum.map(&move_and_broadcast/1)
  end

  ## Helpers

  defp move_and_broadcast(
         {entity, position, %M.AIMovementComponent{path: []} = ai_movement, speed}
       ) do
    %M.AIMovementComponent{orig_x: orig_x, orig_y: orig_y, radius: radius} = ai_movement
    %E.SpeedComponent{value: speed_value} = speed

    # New position
    new_x = Enum.random((orig_x - radius)..(orig_x + radius))
    new_y = Enum.random((orig_y - radius)..(orig_y + radius))
    # path = ....

    # Calc step and delay
    step = max(ceil(speed_value * 0.4), 1)
    delay = trunc(step / (speed_value / 2) * 1250)
    next_move = ElvenGard.ECS.now() + delay

    # Notify all Endpoint on the same map
    event = {
      :entity_move,
      GameService.entity_type(entity),
      GameService.entity_id(entity),
      new_x,
      new_y,
      speed_value
    }

    GameService.System.map_event(event, position)

    # Update components - probably no need for transaction here
    {:ok, _} = Command.update_component(entity, E.PositionComponent, map_x: new_x, map_y: new_y)
    {:ok, _} = Command.update_component(entity, M.AIMovementComponent, next_move: next_move)
  end

  # defp move_and_broadcast({position, ai_movement, speed}) do
  #   %M.AIMovementComponent{orig_x: orig_x, orig_y: orig_y, radius: radius} = ai_movement

  #   new_x = IO.inspect(position)
  #   IO.inspect(ai_movement)
  #   IO.inspect(speed)
  #   IO.puts("-----------")
  # end
end
