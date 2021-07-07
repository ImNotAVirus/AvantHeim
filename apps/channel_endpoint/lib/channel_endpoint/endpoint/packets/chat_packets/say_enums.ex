defmodule ChannelEndpoint.Endpoint.ChatPackets.SayEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum(:color_type,
    white: -1,
    default: 0,
    group: 3,
    grey: 4,
    whisper_name: 5,
    family: 6,
    light_yellow: 7,
    whisper: 8,
    special_gold: 10,
    special_red: 11,
    special_green: 12,
    special_grey: 13,
    effect_bar: 20
  )
end
