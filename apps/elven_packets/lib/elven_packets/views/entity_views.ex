defmodule ElvenPackets.Views.EntityViews do
  @moduledoc """
  TODO: ElvenPackets.Views.EntityViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [required_param: 2]

  alias ElvenPackets.Server.EntityPackets.{CMode, CharSc, Cond, Dir, Eff, St}
  alias GameService.PlayerBundle

  @impl true
  def render(:c_mode, args) do
    entity = required_param(args, :entity)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "c_mode can only be called on players, got: #{inspect(entity)}"
    end

    %CMode{
      entity_type: :character,
      entity_id: GameService.entity_id(entity),
      morph: PlayerBundle.morph(entity),
      morph_upgrade: PlayerBundle.morph_upgrade(entity),
      wings_design: PlayerBundle.wings_design(entity),
      is_arena_winner: PlayerBundle.arena_winner?(entity),
      size: PlayerBundle.size(entity),
      item_morph: PlayerBundle.item_morph(entity)
    }
  end

  ## TODO: Test on PNJ
  def render(:char_sc, args) do
    entity = required_param(args, :entity)

    %CharSc{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      size: entity.__struct__.size(entity)
    }
  end

  def render(:cond, args) do
    entity = required_param(args, :entity)

    %Cond{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      no_attack: not entity.__struct__.can_attack(entity),
      no_move: not entity.__struct__.can_move(entity),
      speed: entity.__struct__.speed(entity)
    }
  end

  def render(:dir, args) do
    entity = required_param(args, :entity)

    %Dir{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      direction: entity.__struct__.direction(entity)
    }
  end

  def render(:eff, args) do
    entity = required_param(args, :entity)
    value = required_param(args, :value)

    %Eff{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      value: value
    }
  end

  def render(:st, args) do
    entity = required_param(args, :entity)
    buffs = required_param(args, :buffs)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "st can only be called on players currently, got: #{inspect(entity)}"
    end

    hp_percent = trunc(PlayerBundle.hp(entity) * 100 / PlayerBundle.hp_max(entity))
    mp_percent = trunc(PlayerBundle.mp(entity) * 100 / PlayerBundle.mp_max(entity))

    %St{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      level: PlayerBundle.level(entity),
      hero_level: PlayerBundle.hero_level(entity),
      hp: PlayerBundle.hp(entity),
      hp_percent: hp_percent,
      mp: PlayerBundle.mp(entity),
      mp_percent: mp_percent,
      # TODO: Buff is a System. Not sure how to do it currently
      # Update 08/09/2023 yes this is a system and I'm pretty sure how to do it :)
      buffs: buffs
    }
  end
end
