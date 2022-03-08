defmodule ElvenEnums.MapEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :spawn_effect_type, [
    :summon,
    :no_effect,
    :falling
  ]

  defenum :portal_type,
    map_portal: -1,
    ts_normal: 0,
    locked: 1,
    open: 2,
    miniland: 3,
    ts_end: 4,
    ts_end_closed: 5,
    exit: 6,
    exit_closed: 7,
    raid: 8,
    # effect -- same as 13 - 19 and 20 - 126
    effect: 9,
    angel_raid: 10,
    demon_raid: 11,
    timespace: 12,
    shop_teleport: 20
end
