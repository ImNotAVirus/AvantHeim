defmodule ElvenPackets.Views.PlayerViews do
  @moduledoc """
  TODO: ElvenPackets.Views.PlayerViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 3, required_param: 2]

  alias ElvenPackets.SubPackets.Player.Family
  alias ElvenPackets.Server.PlayerPackets.{Fd, Fs, Lev, Rsfi, Stat, Tit, CInfo}
  alias GameService.PlayerBundle

  ## Public API

  # FIXME: Move to ElvenGard.Network.View
  def render(name), do: render(name, %{})

  @impl true
  def render(:c_info, args) do
    entity = required_param(args, :entity)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "c_info can only be called on players, got: #{inspect(entity)}"
    end

    family = %Family{
      id: PlayerBundle.family_id(entity),
      rank: PlayerBundle.family_rank(entity),
      name: PlayerBundle.family_name(entity)
    }

    %CInfo{
      character_id: GameService.entity_id(entity),
      name: PlayerBundle.name(entity),
      group_id: PlayerBundle.group_id(entity),
      family: family,
      authority: PlayerBundle.authority(entity),
      gender: PlayerBundle.gender(entity),
      hair_style: PlayerBundle.hair_style(entity),
      hair_color: PlayerBundle.hair_color(entity),
      class: PlayerBundle.class(entity),
      reputation_icon: PlayerBundle.reputation_icon(entity),
      compliment: PlayerBundle.compliment(entity),
      morph: PlayerBundle.morph(entity),
      is_invisible: PlayerBundle.invisible?(entity),
      family_level: PlayerBundle.family_level(entity),
      morph_upgrade: PlayerBundle.morph_upgrade(entity),
      wings_design: PlayerBundle.wings_design(entity),
      is_arena_winner: PlayerBundle.arena_winner?(entity)
    }
  end

  def render(:fd, args) do
    %Fd{
      reputation: required_param(args, :reputation),
      reputation_icon: required_param(args, :reputation_icon),
      dignity: required_param(args, :dignity),
      dignity_icon: required_param(args, :dignity_icon)
    }
  end

  def render(:fs, args) do
    %Fs{faction: required_param(args, :faction)}
  end

  def render(:lev, args) do
    entity = required_param(args, :entity)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "lev can only be called on players, got: #{inspect(entity)}"
    end

    %Lev{
      level: PlayerBundle.level(entity),
      level_xp: PlayerBundle.level_xp(entity),
      level_xp_max: PlayerBundle.level_xp_max(entity),
      job_level: PlayerBundle.job_level(entity),
      job_level_xp: PlayerBundle.job_level_xp(entity),
      job_level_xp_max: PlayerBundle.job_level_xp_max(entity),
      hero_level: PlayerBundle.hero_level(entity),
      hero_level_xp: PlayerBundle.hero_level_xp(entity),
      hero_level_xp_max: PlayerBundle.hero_level_xp_max(entity),
      reputation: PlayerBundle.reputation(entity),
      cp: PlayerBundle.cp(entity)
    }
  end

  def render(:rsfi, args) do
    act = optional_param(args, :act, 1)
    act_part = optional_param(args, :act_part, 1)
    ts = optional_param(args, :ts, 0)
    ts_max = optional_param(args, :ts_max, 0)

    %Rsfi{act: act, act_part: act_part, ts: ts, ts_max: ts_max}
  end

  def render(:stat, args) do
    entity = required_param(args, :entity)
    option = optional_param(args, :option, 0)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "stat can only be called on players, got: #{inspect(entity)}"
    end

    %Stat{
      hp: PlayerBundle.hp(entity),
      hp_max: PlayerBundle.hp_max(entity),
      mp: PlayerBundle.mp(entity),
      mp_max: PlayerBundle.mp_max(entity),
      option: option
    }
  end

  def render(:tit, args) do
    %Tit{
      class: required_param(args, :class),
      name: required_param(args, :name)
    }
  end
end
