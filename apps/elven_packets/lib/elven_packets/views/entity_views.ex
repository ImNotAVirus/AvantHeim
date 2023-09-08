defmodule ElvenPackets.Views.EntityViews do
  @moduledoc """
  TODO: ElvenPackets.Views.EntityViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 3, required_param: 2]

  alias ElvenPackets.Server.EntityPackets.{CMode, CharSc, Cond, Dir, Eff, St}

  @impl true
  def render(:c_mode, args) do
    character = required_param(args, :character)

    %CMode{
      entity_type: :character,
      entity_id: character.id,
      morph: FakeData.morph(character_id: character.id),
      morph_upgrade: FakeData.morph_upgrade(character_id: character.id),
      wings_design: FakeData.wings_design(character_id: character.id),
      is_arena_winner: FakeData.is_arena_winner(character_id: character.id),
      size: FakeData.size(character_id: character.id),
      item_morph: FakeData.item_morph(character_id: character.id)
    }
  end

  ## TODO: Test on PNJ
  def render(:char_sc, args) do
    entity = required_param(args, :entity)

    %CharSc{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      size: MapEntity.size(entity)
    }
  end

  def render(:cond, args) do
    entity = required_param(args, :entity)

    %Cond{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      no_attack: not BattleEntity.can_attack(entity),
      no_move: not BattleEntity.can_move(entity),
      speed: MapEntity.speed(entity)
    }
  end

  def render(:dir, args) do
    entity = required_param(args, :entity)

    %Dir{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      direction: MapEntity.direction(entity)
    }
  end

  def render(:eff, args) do
    entity = required_param(args, :entity)
    value = required_param(args, :value)

    %Eff{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      value: value
    }
  end

  def render(:st, args) do
    entity = required_param(args, :entity)
    buffs = optional_param(args, :buffs, [])

    %St{
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
end
