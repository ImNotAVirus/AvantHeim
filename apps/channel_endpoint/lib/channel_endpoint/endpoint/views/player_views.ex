defmodule ChannelEndpoint.Endpoint.PlayerViews do
  @moduledoc """
  TODO: Documentation
  """

  alias FakeData.Character

  alias ChannelEndpoint.Endpoint.PlayerPackets.{
    CInfo,
    Fd,
    Fs,
    Lev,
    Rsfi,
    Stat,
    Tit
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:rsfi, %Character{}), do: %Rsfi{}

  def render(:tit, %Character{} = character) do
    %Tit{class: character.class, name: character.name}
  end

  def render(:fs, %Character{} = character) do
    %Fs{faction: character.faction}
  end

  def render(:fd, %Character{} = character) do
    %Fd{
      reputation: FakeData.reputation(character_id: character.id),
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      dignity: FakeData.dignity(character_id: character.id),
      dignity_icon_id: FakeData.dignity_icon_id(character_id: character.id)
    }
  end

  def render(:lev, %Character{} = character) do
    %Lev{
      level: character.level,
      level_xp: character.level_xp,
      level_xp_max: FakeData.level_xp_max(character_id: character.id),
      job_level: character.job_level,
      job_level_xp: character.job_level_xp,
      job_level_xp_max: FakeData.job_level_xp_max(character_id: character.id),
      hero_level: character.hero_level,
      hero_level_xp: character.hero_level_xp,
      hero_level_xp_max: FakeData.hero_level_xp_max(character_id: character.id),
      reputation: FakeData.reputation(character_id: character.id),
      cp: FakeData.cp(character_id: character.id)
    }
  end

  def render(:stat, %Character{} = character) do
    %Stat{
      hp: FakeData.hp(character_id: character.id),
      hp_max: FakeData.hp_max(character_id: character.id),
      mp: FakeData.mp(character_id: character.id),
      mp_max: FakeData.mp_max(character_id: character.id)
    }
  end

  def render(:c_info, %Character{} = character) do
    %CInfo{
      character_id: character.id,
      name: character.name,
      group_id: FakeData.group_id(character_id: character.id),
      family_id: FakeData.family_id(character_id: character.id),
      family_rank: FakeData.family_rank(character_id: character.id),
      family_name: FakeData.family_name(character_id: character.id),
      name_color_id: FakeData.name_color_id(character_id: character.id),
      gender: character.gender,
      hair_style: character.hair_style,
      hair_color: character.hair_color,
      class: character.class,
      reputation_icon_id: FakeData.reputation_icon_id(character_id: character.id),
      compliment: FakeData.compliment(character_id: character.id),
      morph: FakeData.morph(character_id: character.id),
      invisible: FakeData.invisible(character_id: character.id),
      family_level: FakeData.family_level(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      arena_winner: FakeData.arena_winner(character_id: character.id)
    }
  end
end
