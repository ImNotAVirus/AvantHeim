defmodule ElvenPackets.Enums.AreaEnums do
  @moduledoc """
  TODO: ElvenPackets.Enums.AreaEnums
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :guri_type,
    emoji: 10,
    scene_req_act1: 40,
    scene_req_act2: 41,
    scene_req_act3: 42,
    scene_req_act4: 43,
    scene_req_act5: 44

  defenum :entity_type,
    player: 1,
    monster: 2,
    npc: 3

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
