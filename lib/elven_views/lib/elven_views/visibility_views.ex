defmodule ElvenViews.VisibilityViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.Entity.EntityPosition

  alias ElvenViews.SubPackets.EquipmentSubPacket
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket
  alias ElvenViews.SubPackets.ItemUpgradeRaritySubPacket

  alias ElvenViews.VisibilityPackets.{InCharacterPacket, OutPacket}

  ## Public API

  @impl true
  def render(:in, args) do
    entity = required_param(args, :entity)

    case Entity.type(entity) do
      :character -> in_character_packet(entity, args)
    end
  end

  def render(:out, args) do
    entity = required_param(args, :entity)

    %OutPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity)
    }
  end

  ## Helpers

  defp in_character_packet(character, args) do
    equipments = required_param(args, :equipments)
    spawn_effect = optional_param(args, :spawn_effect, :summon)

    [
      hat,
      armor,
      main_weapon,
      secondary_weapon,
      mask,
      fairy,
      costume_suit,
      costume_hat,
      weapon_skin,
      wings_skin
    ] = equipments

    equipment_packet = %EquipmentSubPacket{
      hat: hat,
      armor: armor,
      main_weapon: main_weapon,
      secondary_weapon: secondary_weapon,
      mask: mask,
      fairy: fairy,
      costume_suit: costume_suit,
      costume_hat: costume_hat,
      weapon_skin: weapon_skin,
      wings_skin: wings_skin
    }

    family_id_rank = %FamilyIdRankSubPacket{
      id: FakeData.family_id(character_id: character.id),
      rank: FakeData.family_rank(character_id: character.id)
    }

    weapon_upgrade = %ItemUpgradeRaritySubPacket{upgrade: 0, rarity: 0}
    armor_upgrade = %ItemUpgradeRaritySubPacket{upgrade: 0, rarity: 0}

    hp = FakeData.hp(character_id: character.id)
    hp_max = FakeData.hp_max(character_id: character.id)
    mp = FakeData.mp(character_id: character.id)
    mp_max = FakeData.mp_max(character_id: character.id)
    hp_percent = trunc(hp * 100 / hp_max)
    mp_percent = trunc(mp * 100 / mp_max)

    %EntityPosition{
      map_x: map_x,
      map_y: map_y
    } = MapEntity.position(character)

    %InCharacterPacket{
      name: character.name,
      entity_id: Entity.id(character),
      map_x: map_x,
      map_y: map_y,
      direction: MapEntity.direction(character),
      authority: FakeData.authority(character_id: character.id),
      gender: character.gender,
      hair_style: character.hair_style,
      hair_color: character.hair_color,
      class: character.class,
      equipments: equipment_packet,
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      is_sitting: MapEntity.is_sitting(character),
      group_id: FakeData.group_id(character_id: character.id),
      fairy_move_type_id: FakeData.fairy_move_type_id(character_id: character.id),
      fairy_element: FakeData.fairy_element(character_id: character.id),
      fairy_morph: FakeData.fairy_morph(character_id: character.id),
      spawn_effect: spawn_effect,
      morph: FakeData.morph(character_id: character.id),
      weapon_upgrade: weapon_upgrade,
      armor_upgrade: armor_upgrade,
      family_id_rank: family_id_rank,
      family_name: FakeData.family_name(character_id: character.id),
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      is_invisible: FakeData.is_invisible(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      faction: character.faction,
      morph_design: FakeData.morph_design(character_id: character.id),
      level: character.level,
      family_level: FakeData.family_level(character_id: character.id),
      family_icons: FakeData.family_icons(character_id: character.id),
      is_arena_winner: FakeData.is_arena_winner(character_id: character.id),
      compliment: FakeData.compliment(character_id: character.id),
      size: FakeData.size(character_id: character.id),
      hero_level: character.hero_level,
      title_id: FakeData.title_id(character_id: character.id)
    }
  end
end
