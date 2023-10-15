defmodule GameService.AIMovementSystem do
  @moduledoc """
  TODO: Documentation for GameService.AIMovementSystem

  TODO: Write tests for AIMovementSystem lol
  TODO: Listen on EntityMapEnter and EntityMapLeave to know when there
  is no more player on the map
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
  def run(%{partition: map_ref}) do
    now = ElvenGard.ECS.now()

    # TODO: Maybe create a module for the map grid put/get
    grid = :persistent_term.get({:map_grid, map_ref})

    # Get monster that need to move
    {ElvenGard.ECS.Entity, E.PositionComponent, M.AIMovementComponent, E.SpeedComponent}
    # FIXME: Why this doesn't work wtf
    # |> Query.select(with: [{M.AIMovementComponent, [{:>=, :next_move, now}]}])
    |> Query.select(
      with: [M.AIMovementComponent, {E.PositionComponent, [{:==, :map_ref, map_ref}]}]
    )
    |> Query.all()
    |> Enum.filter(&(elem(&1, 2).next_move <= now))
    |> Enum.filter(&(elem(&1, 3).value > 0))
    |> Enum.map(&move_and_broadcast(&1, grid))
  end

  ## Helpers

  # If Entity is not in movement
  defp move_and_broadcast(
         {entity, position, %M.AIMovementComponent{path: []} = ai_movement, speed},
         {width, height, map_grid} = map
       ) do
    %E.PositionComponent{map_x: map_x, map_y: map_y} = position
    %M.AIMovementComponent{orig_x: orig_x, orig_y: orig_y, radius: radius} = ai_movement

    # New position
    new_x = Enum.random((orig_x - radius)..(orig_x + radius))
    new_y = Enum.random((orig_y - radius)..(orig_y + radius))

    case AStar.path(map_grid, width, height, {map_x, map_y}, {new_x, new_y}) do
      # If Entity can move
      {:ok, [_ | _] = path} ->
        new_ai_movement = %M.AIMovementComponent{ai_movement | path: path}
        move_and_broadcast({entity, position, new_ai_movement, speed}, map)

      # Else do nothing
      _ ->
        :ok
    end
  end

  defp move_and_broadcast({entity, position, ai_movement, speed}, _map) do
    %E.SpeedComponent{value: speed_value} = speed
    %M.AIMovementComponent{path: path} = ai_movement

    # Calc step and delay
    step =
      ceil(speed_value * 0.4)
      # max = length(path)
      |> min(length(path))

    delay = trunc(step / (speed_value / 2) * 1250)
    next_move = ElvenGard.ECS.now() + delay

    # Get our new position and new path
    {new_x, new_y} = Enum.at(path, step - 1)
    new_path = Enum.drop(path, step)

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
    next_move = if match?([], new_path), do: next_move + Enum.random(1500..4500), else: next_move
    new_mov_attrs = [next_move: next_move, path: new_path]
    {:ok, _} = Command.update_component(entity, M.AIMovementComponent, new_mov_attrs)
    {:ok, _} = Command.update_component(entity, E.PositionComponent, map_x: new_x, map_y: new_y)
  end
end
