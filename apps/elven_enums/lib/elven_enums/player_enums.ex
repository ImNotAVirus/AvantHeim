defmodule ElvenEnums.PlayerEnums do
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
    martial_artist: 4

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

  defenum :family_rank,
    member: 0,
    keeper: 1,
    deputy: 2,
    head: 3

  defenum :morph,
    pyjama: 1,
    warrior: 2,
    ninja: 3,
    ranger: 4,
    assassin: 5,
    red_magician: 6,
    holy_mage: 7,
    chicken: 8,
    jajamaru: 9,
    crusader: 10,
    berserker: 11,
    destroyer: 12,
    wild_keeper: 13,
    blue_mage: 14,
    dark_gunner: 15,
    pirate: 16,
    gladiator: 17,
    cannoneer: 18,
    volcano: 19,
    battle_monk: 20,
    scout: 21,
    tide_lord: 22,
    death_reaper: 23,
    demon_hunter: 24,
    seer: 25,
    renegade: 26,
    avenging_angel: 27,
    archmage: 28,
    draconic_fist: 29,
    draconic_fist_transformed: 30,
    mystic_arts: 31,
    wedding: 32,
    master_wolf: 33,
    demon_warrior: 34,
    angler: 35,
    angler_skin: 36,
    chef_skin: 37,
    chef: 38,
    waterfall_berserker: 39,
    sunchaser: 40,
    voodoo_priest: 41,
    flame_druid: 42,
    flame_druid_transformed: 43,
    dragon_knight: 45,
    blaster: 46,
    gravity: 47,
    hydraulic_fist: 48,
    pet_trainer: 49,
    pet_tariner_skin: 50,
    merling: 1003000

  defenum :morph_design,
    angel_wings: 1,
    devil_wings: 2,
    fire_wings: 3,
    ice_wings: 4,
    golden_eagle_wings: 5,
    titan_wings: 6,
    archangel_wings: 7,
    archdaemon_wings: 8,
    blazing_fire_wings: 9,
    frosty_ice_wings: 10,
    golden_wings: 11,
    onyx_wings: 12,
    fairy_wings: 13,
    mega_titan_wings: 14,
    zephyr_wings: 15,
    lightning_wings: 16,
    blade_wings: 17,
    crystal_wings: 18,
    petal_wings: 19,
    lunar_wings: 20,
    green_retro_wings: 21,
    pink_retro_wings: 22,
    yellow_retro_wings: 23,
    purple_retro_wings: 24,
    red_retro_wings: 25,
    magenta_retro_wings: 26,
    cyan_retro_wings: 27,
    tree_wings: 28
end
