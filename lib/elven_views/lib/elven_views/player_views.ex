defmodule ElvenViews.PlayerViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenCaching.Entity.Character

  alias ElvenViews.SubPackets.FamilyIdRankSubPacket

  alias ElvenViews.PlayerPackets.{
    CInfoPacket,
    FdPacket,
    FsPacket,
    LevPacket,
    RsfiPacket,
    StatPacket,
    TitPacket
  }

  ## Public API

  @impl true
  def render(:rsfi, args) do
    act = optional_param(args, :act, 1)
    act_part = optional_param(args, :act_part, 1)
    ts = optional_param(args, :ts, 0)
    ts_max = optional_param(args, :ts_max, 0)

    %RsfiPacket{act: act, act_part: act_part, ts: ts, ts_max: ts_max}
  end

  def render(:tit, args) do
    character = required_param(args, :character)

    %TitPacket{class: character.class, name: character.name}
  end

  def render(:fs, args) do
    character = required_param(args, :character)

    %FsPacket{faction: character.faction}
  end

  def render(:fd, args) do
    character = required_param(args, :character)

    %FdPacket{
      reputation: FakeData.reputation(character_id: character.id),
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      dignity: FakeData.dignity(character_id: character.id),
      dignity_icon_id: FakeData.dignity_icon_id(character_id: character.id)
    }
  end

  def render(:lev, args) do
    character = required_param(args, :character)

    %LevPacket{
      level: character.level,
      level_xp: character.level_xp,
      level_xp_max: FakeData.level_xp_max(character_id: character.id),
      job_level: character.job_level,
      job_level_xp: character.job_level_xp,
      job_level_xp_max: FakeData.job_level_xp_max(character_id: character.id),
      hero_level: FakeData.hero_level(character_id: character.id),
      hero_level_xp: FakeData.hero_level_xp(character_id: character.id),
      hero_level_xp_max: FakeData.hero_level_xp_max(character_id: character.id),
      reputation: FakeData.reputation(character_id: character.id),
      cp: FakeData.cp(character_id: character.id)
    }
  end

  def render(:stat, args) do
    character = required_param(args, :character)
    option = optional_param(args, :option, 0)

    %StatPacket{
      hp: FakeData.hp(character_id: character.id),
      hp_max: FakeData.hp_max(character_id: character.id),
      mp: FakeData.mp(character_id: character.id),
      mp_max: FakeData.mp_max(character_id: character.id),
      option: option
    }
  end

  def render(:c_info, args) do
    character = required_param(args, :character)

    family_id_rank = %FamilyIdRankSubPacket{
      id: FakeData.family_id(character_id: character.id),
      rank: FakeData.family_rank(character_id: character.id)
    }

    %CInfoPacket{
      character_id: character.id,
      name: character.name,
      group_id: FakeData.group_id(character_id: character.id),
      family_id_rank: family_id_rank,
      family_name: FakeData.family_name(character_id: character.id),
      authority: FakeData.authority(character_id: character.id),
      gender: character.gender,
      hair_style: character.hair_style,
      hair_color: character.hair_color,
      class: character.class,
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      compliment: FakeData.compliment(character_id: character.id),
      morph: FakeData.morph(character_id: character.id),
      is_invisible: FakeData.is_invisible(character_id: character.id),
      family_level: FakeData.family_level(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      morph_design: FakeData.morph_design(character_id: character.id),
      is_arena_winner: FakeData.is_arena_winner(character_id: character.id)
    }
  end
end
