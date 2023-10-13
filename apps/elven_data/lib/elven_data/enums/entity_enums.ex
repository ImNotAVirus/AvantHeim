defmodule ElvenData.Enums.EntityEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  # Type portal is unknown yet
  defenum :entity_type, player: 1, npc: 2, monster: 3, map_object: 9, portal: 1000
  defenum :element_type, [:neutral, :fire, :water, :light, :darkness]
  defenum :fairy_move_type, static: 1, circle: 2, slow_circle: 3, big_circle: 4

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

  defenum :fairy_morph_type, [
    :default,
    :dancing_chicky,
    :little_chick,
    :grumbly_chicky,
    :solaris,
    :sellaim,
    :woondine,
    :eperial,
    :turik,
    :azuris,
    :sellaim_boosted,
    :woondine_boosted,
    :eperial_boosted,
    :turik_boosted,
    :red_magical,
    :princess,
    :no_element,
    :no_element_2,
    :no_element_3,
    :no_element_4,
    :default_2,
    :mini_sellaim,
    :mini_woondine,
    :mini_eperial,
    :mini_turik,
    :mini_princess,
    :mini_sellaim_boosted,
    :mini_woondine_boosted,
    :mini_eperial_boosted,
    :mini_turik_boosted,
    :default_3,
    :mini_sellaim_2,
    :mini_woondine_2,
    :mini_eperial_2,
    :mini_turik_2,
    :mini_princess_2,
    :medium_sellaim_boosted,
    :medium_woondine_boosted,
    :medium_eperial_boosted,
    :medium_turik_boosted,
    :elkaim,
    :ladine,
    :rumial,
    :varik,
    :no_element_5,
    :elkaim_boosted,
    :ladine_boosted,
    :rumial_boosted,
    :varik_boosted,
    :zenas,
    :erenia,
    :fernon,
    :no_element_6,
    :no_element_7,
    :zenas_boosted,
    :erenia_boosted,
    :fernon_boosted
  ]
end
