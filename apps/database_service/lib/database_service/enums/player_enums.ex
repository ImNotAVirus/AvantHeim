defmodule DatabaseService.PlayerEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  ## Account enums
  defenum :language, [:fr, :en]
  defenum :authority, player: 0, game_master: 2, administrator: 10

  ## Character enums
  defenum :gender, male: 0, female: 1

  defenum :faction,
    neutral: 0,
    angel: 1,
    demon: 2

  defenum :miniland_state,
    open: 0,
    private: 1,
    lock: 2

  defenum :character_class,
    adventurer: 0,
    swordman: 1,
    archer: 2,
    magician: 3,
    wrestler: 4

  defenum :hair_style,
    hair_style_a: 0,
    hair_style_b: 1,
    hair_style_c: 2,
    hair_style_d: 3,
    no_hair: 4

  defenum :hair_color,
    dark_purple: 0,
    yellow: 1,
    blue: 2,
    purple: 3,
    orange: 4,
    brown: 5,
    green: 6,
    dark_grey: 7,
    light_blue: 8,
    pink_red: 9
end
