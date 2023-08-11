defmodule ElvenPackets.Enums.LobbyEnums do
  @moduledoc """
  TODO: ElvenPackets.Enums.LobbyEnums
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :gender, [
    :male,
    :female
  ]

  defenum :hair_style, [
    :hair_style_a,
    :hair_style_b,
    :hair_style_c,
    :hair_style_d,
    :no_hair
  ]

  defenum :hair_color, [
    :dark_purple,
    :yellow,
    :blue,
    :purple,
    :orange,
    :brown,
    :green,
    :dark_grey,
    :light_blue,
    :pink_red
  ]
end
