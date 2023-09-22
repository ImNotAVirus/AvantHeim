defmodule ElvenData.Enums.PlayerEnums do
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

  defenum :morph,
    default: 0,
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
    pet_trainer_skin: 50,
    merling: 1_003_000

  defenum :wings_design,
    default: 0,
    angel: 1,
    devil: 2,
    fire: 3,
    ice: 4,
    golden_eagle: 5,
    titan: 6,
    archangel: 7,
    archdaemon: 8,
    blazing_fire: 9,
    frosty_ice: 10,
    golden: 11,
    onyx: 12,
    fairy: 13,
    mega_titan: 14,
    zephyr: 15,
    lightning: 16,
    blade: 17,
    crystal: 18,
    petal: 19,
    lunar: 20,
    green_retro: 21,
    pink_retro: 22,
    yellow_retro: 23,
    purple_retro: 24,
    red_retro: 25,
    magenta_retro: 26,
    cyan_retro: 27,
    tree: 28

  defenum :family_rank,
    member: 0,
    keeper: 1,
    deputy: 2,
    head: 3

  defenum :dignity_icon,
    basic: 1,
    suspected: 2,
    bluffed_name_only: 3,
    not_qualified_for: 4,
    useless: 5,
    stupid_minded: 6

  defenum :reputation_icon,
    stupid_minded: -6,
    useless: -5,
    not_qualified_for: -4,
    bluffed_name_only: -3,
    suspected: -2,
    basic: -1,
    green_beginner: 1,
    blue_beginner: 2,
    red_beginner: 3,
    green_trainee: 4,
    blue_trainee: 5,
    red_trainee: 6,
    green_experienced: 7,
    blue_experienced: 8,
    red_experienced: 9,
    green_soldier: 10,
    blue_soldier: 11,
    red_soldier: 12,
    green_expert: 13,
    blue_expert: 14,
    red_expert: 15,
    green_leader: 16,
    blue_leader: 17,
    red_leader: 18,
    green_master: 19,
    blue_master: 20,
    red_master: 21,
    green_nos: 22,
    blue_nos: 23,
    red_nos: 24,
    green_elite: 25,
    blue_elite: 26,
    red_elite: 27,
    green_legend: 28,
    blue_legend: 29,
    ancient_hero: 30,
    mysterious_hero: 31,
    legendary_hero: 32
end