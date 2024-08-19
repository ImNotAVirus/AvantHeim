defmodule ElvenData.Enums.MapEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :spawn_effect_type, [
    :summon,
    :no_effect,
    :falling
  ]

  defenum :portal_direction_type, [
    :north,
    :east,
    :south,
    :west
  ]
end
