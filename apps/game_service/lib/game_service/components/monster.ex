defmodule GameService.MonsterComponents do
  @moduledoc """
  TODO: Documentation for GameService.MonsterComponents
  """

  defmodule MonsterComponent do
    use ElvenGard.ECS.Component, state: [:name, :vnum, :spawn_effect]
  end
end
