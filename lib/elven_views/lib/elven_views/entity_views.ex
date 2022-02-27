defmodule ElvenViews.EntityViews do
  @moduledoc """
  TODO: Documentation
  """

  import ElvenViews, only: [optional_param: 2, required_param: 2]

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.BattleEntity
  alias ElvenCaching.LevelableEntity

  alias ElvenViews.EntityPackets.{
    CharSc,
    Cond,
    Eff,
    St,
    Dir
  }

  @behaviour ElvenViews

  ## Public API

  @impl true
  def render(:st, args) do
    entity = required_param(args, :entity)
    buffs = optional_param(args, :buffs, [])

    %St{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      level: LevelableEntity.level(entity),
      hero_level: LevelableEntity.hero_level(entity),
      hp: BattleEntity.hp(entity),
      hp_max: BattleEntity.hp_max(entity),
      mp: BattleEntity.mp(entity),
      mp_max: BattleEntity.mp_max(entity),
      # TODO: Buff is a System. Not sure how to do it currently
      buffs: buffs
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

  def render(:eff, args) do
    entity = required_param(args, :entity)
    value = required_param(args, :value)

    %Eff{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      value: value
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
end
