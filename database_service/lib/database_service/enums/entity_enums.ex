defmodule DatabaseService.EntityEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  # Type portal is unknown yet
  defenum :entity_type, character: 1, npc: 2, monster: 3, map_object: 9, portal: 1000
  defenum :element_type, [:neutral, :fire, :water, :light, :darkness]

  defenum :direction_type, [
    :north,
    :east,
    :south,
    :west,
    :north_east,
    :south_east,
    :south_west,
    :north_west
  ]
end
