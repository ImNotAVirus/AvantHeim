defmodule ElvenPackets.Views.VisibilityViews do
  @moduledoc """
  TODO: ElvenPackets.Views.VisibilityViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [required_param: 2, optional_param: 3]

  alias ElvenPackets.Server.VisibilityPackets.{InPlayer, InNpcOrMonster, Out}
  alias ElvenPackets.SubPackets.Equipment
  alias ElvenPackets.SubPackets.Item.UpgradeRarity
  alias ElvenPackets.SubPackets.Player.Family
  alias GameService.{MonsterBundle, PlayerBundle}

  ## Renders

  @impl true
  def render(:in, args) do
    entity = required_param(args, :entity)

    case entity.__struct__ do
      PlayerBundle -> in_player_packet(entity, args)
      MonsterBundle -> in_monster_packet(entity, args)
    end
  end

  def render(:out, args) do
    %Out{
      entity_type: required_param(args, :entity_type),
      entity_id: required_param(args, :entity_id)
    }
  end

  ## Helpers

  defp in_player_packet(entity, args) do
    spawn_effect = optional_param(args, :spawn_effect, :summon)

    # FIXME: Hardcoded until equipment system
    equipment_packet = %Equipment{}
    weapon_upgrade = %UpgradeRarity{upgrade: 0, rarity: 0}
    armor_upgrade = %UpgradeRarity{upgrade: 0, rarity: 0}

    family = %Family{
      id: PlayerBundle.family_id(entity),
      rank: PlayerBundle.family_rank(entity),
      name: PlayerBundle.family_name(entity)
    }

    hp_percent = trunc(PlayerBundle.hp(entity) * 100 / PlayerBundle.hp_max(entity))
    mp_percent = trunc(PlayerBundle.mp(entity) * 100 / PlayerBundle.mp_max(entity))

    %InPlayer{
      name: PlayerBundle.name(entity),
      entity_id: GameService.entity_id(entity),
      map_x: PlayerBundle.map_x(entity),
      map_y: PlayerBundle.map_y(entity),
      direction: PlayerBundle.direction(entity),
      authority: PlayerBundle.authority(entity),
      gender: PlayerBundle.gender(entity),
      hair_style: PlayerBundle.hair_style(entity),
      hair_color: PlayerBundle.hair_color(entity),
      class: PlayerBundle.class(entity),
      equipments: equipment_packet,
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      is_sitting: PlayerBundle.sitting?(entity),
      group_id: PlayerBundle.group_id(entity),
      fairy_move_type_id: PlayerBundle.fairy_move_type_id(entity),
      fairy_element: PlayerBundle.fairy_element(entity),
      fairy_morph: PlayerBundle.fairy_morph(entity),
      spawn_effect: spawn_effect,
      morph: PlayerBundle.morph(entity),
      weapon_upgrade: weapon_upgrade,
      armor_upgrade: armor_upgrade,
      family: family,
      reputation_icon: PlayerBundle.reputation_icon(entity),
      is_invisible: PlayerBundle.invisible?(entity),
      morph_upgrade: PlayerBundle.morph_upgrade(entity),
      faction: PlayerBundle.faction(entity),
      wings_design: PlayerBundle.wings_design(entity),
      level: PlayerBundle.level(entity),
      family_level: PlayerBundle.family_level(entity),
      family_icons: PlayerBundle.family_icons(entity),
      is_arena_winner: PlayerBundle.arena_winner?(entity),
      compliment: PlayerBundle.compliment(entity),
      size: PlayerBundle.size(entity),
      hero_level: PlayerBundle.hero_level(entity),
      title_id: PlayerBundle.title_id(entity)
    }
  end

  defp in_monster_packet(entity, args) do
    hp_percent = trunc(MonsterBundle.hp(entity) * 100 / MonsterBundle.hp_max(entity))
    mp_percent = trunc(MonsterBundle.mp(entity) * 100 / MonsterBundle.mp_max(entity))

    %InNpcOrMonster{
      entity_type: :monster,
      entity_id: GameService.entity_id(entity),
      vnum: MonsterBundle.vnum(entity),
      map_x: MonsterBundle.map_x(entity),
      map_y: MonsterBundle.map_y(entity),
      direction: MonsterBundle.map_y(direction),
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      is_sitting: MonsterBundle.sitting?(entity),
      is_invisible: MonsterBundle.invisible?(entity),
      name: MonsterBundle.name(entity),
      spawn_effect: MonsterBundle.spawn_effect(entity)
    }
  end
end
