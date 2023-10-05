defmodule GameService.MonsterComponents do
  @moduledoc """
  TODO: Documentation for GameService.MonsterComponents
  """

  defmodule MonsterComponent do
    use ElvenGard.ECS.Component, state: [:name, :vnum, :spawn_effect]
  end

  defmodule AIMovementComponent do
    use ElvenGard.ECS.Component,
      state: [orig_x: nil, orig_y: nil, radius: 2, next_move: nil, path: []]
  end
end
