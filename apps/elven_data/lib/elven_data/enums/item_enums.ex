defmodule ElvenData.Enums.ItemEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  ## Enums

  defenum :inventory_type,
    # equipped: not present in the game, for DB purpose
    equipped: -1,
    equipment: 0,
    main: 1,
    etc: 2,
    miniland: 3,
    specialist: 6,
    costume: 7

  defenum :slot_type, [
    :main_weapon,
    :armor,
    :hat,
    :gloves,
    :boots,
    :secondary_weapon,
    :necklace,
    :ring,
    :bracelet,
    :mask,
    :fairy,
    :amulet,
    :sp,
    :costume_suit,
    :costume_hat,
    :weapon_skin,
    :wings
  ]
end
