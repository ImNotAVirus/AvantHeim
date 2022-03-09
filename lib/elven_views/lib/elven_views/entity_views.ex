defmodule ElvenViews.EntityViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.BattleEntity
  alias ElvenCaching.BattleEntityHelper
  alias ElvenCaching.LevelableEntity

  alias ElvenViews.EntityPackets.{
    CharScPacket,
    CondPacket,
    EffPacket,
    StPacket,
    DirPacket,
    CModePacket
  }

  ## Public API

  @impl true
  def render(:st, args) do
    entity = required_param(args, :entity)
    buffs = optional_param(args, :buffs, [])

    %StPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      level: LevelableEntity.level(entity),
      hero_level: LevelableEntity.hero_level(entity),
      hp: BattleEntity.hp(entity),
      hp_percent: BattleEntityHelper.hp_percent(entity),
      mp: BattleEntity.mp(entity),
      mp_percent: BattleEntityHelper.mp_percent(entity),
      # TODO: Buff is a System. Not sure how to do it currently
      buffs: buffs
    }
  end

  ## TODO: Test on PNJ
  def render(:char_sc, args) do
    entity = required_param(args, :entity)

    %CharScPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      size: MapEntity.size(entity)
    }
  end

  def render(:cond, args) do
    entity = required_param(args, :entity)

    %CondPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      no_attack: not BattleEntity.can_attack(entity),
      no_move: not BattleEntity.can_move(entity),
      speed: MapEntity.speed(entity)
    }
  end

  def render(:eff, args) do
    entity = required_param(args, :entity)
    value = required_param(args, :value)

    %EffPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      value: value
    }
  end

  def render(:dir, args) do
    entity = required_param(args, :entity)

    %DirPacket{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      direction: MapEntity.direction(entity)
    }
  end

  def render(:c_mode, args) do
    character = required_param(args, :character)

    %CModePacket{
      entity_type: :character,
      entity_id: character.id,
      morph: FakeData.morph(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      morph_design: FakeData.morph_design(character_id: character.id),
      is_arena_winner: FakeData.is_arena_winner(character_id: character.id),
      size: FakeData.size(character_id: character.id),
      item_morph: FakeData.item_morph(character_id: character.id)
    }
  end
end
