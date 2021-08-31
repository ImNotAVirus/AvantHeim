defmodule ChannelEndpoint.Endpoint.VisibilityViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Position
  alias CachingService.Player.Character
  alias CachingService.Map.Monster
  alias ChannelEndpoint.Endpoint.VisibilityPackets.{InCharacter, InNpcOrMonster}

  ## Public API

  @spec render(atom, any) :: any
  def render(:in, %Character{} = character) do
    %Position{
      map_x: map_x,
      map_y: map_y
    } = Character.get_position(character)

    hp = FakeData.hp(character_id: character.id)
    hp_max = FakeData.hp_max(character_id: character.id)
    mp = FakeData.mp(character_id: character.id)
    mp_max = FakeData.mp_max(character_id: character.id)
    hp_percent = trunc(hp * 100 / hp_max)
    mp_percent = trunc(mp * 100 / mp_max)

    %InCharacter{
      name: character.name,
      character_id: character.id,
      map_x: map_x,
      map_y: map_y,
      direction: FakeData.direction(character_id: character.id),
      name_color_id: FakeData.name_color_id(character_id: character.id),
      gender: character.gender,
      hair_style: character.hair_style,
      hair_color: character.hair_color,
      class: character.class,
      equipments: FakeData.equipments(character_id: character.id),
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      is_sitting: false,
      group_id: FakeData.group_id(character_id: character.id),
      fairy_move_type_id: FakeData.fairy_move_type_id(character_id: character.id),
      fairy_element: FakeData.fairy_element(character_id: character.id),
      fairy_morph: FakeData.fairy_morph(character_id: character.id),
      morph: FakeData.morph(character_id: character.id),
      weapon_upgrade: "00",
      armor_upgrade: "00",
      family_id: FakeData.family_id(character_id: character.id),
      family_rank: FakeData.family_rank(character_id: character.id),
      family_name: FakeData.family_name(character_id: character.id),
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      is_invisible: FakeData.is_invisible(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      faction: character.faction,
      morph_design: FakeData.morph_design(character_id: character.id),
      level: character.level,
      family_level: FakeData.family_level(character_id: character.id),
      family_icon_ids: FakeData.family_icon_ids(character_id: character.id),
      is_arena_winner: FakeData.is_arena_winner(character_id: character.id),
      compliment: FakeData.compliment(character_id: character.id),
      hero_level: FakeData.hero_level(character_id: character.id),
      size: FakeData.size(character_id: character.id),
      title_id: FakeData.title_id(character_id: character.id)
    }
  end

  def render(:in, %Monster{} = monster) do
    %InNpcOrMonster{
      entity_type: :monster,
      vnum: monster.vnum,
      id: monster.id,
      map_x: monster.map_x,
      map_y: monster.map_y,
      direction: monster.direction,
      hp_percent: trunc(monster.hp * 100 / monster.hp_max),
      mp_percent: trunc(monster.mp * 100 / monster.mp_max),
      is_sitting: monster.is_sitting,
      is_invisible: monster.is_invisible,
      name: monster.name,
      spawn_effect: :falling
    }
  end
end
