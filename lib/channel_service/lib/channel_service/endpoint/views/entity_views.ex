defmodule ChannelService.Endpoint.EntityViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.BattleEntity
  alias ElvenCaching.LevelableEntity

  alias ChannelService.Endpoint.EntityPackets.{
    CharSc,
    Cond,
    Eff,
    St,
    Dir
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:st, entity) do
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
      buffs: []
    }
  end

  ## TODO: Test on PNJ
  def render(:char_sc, entity) do
    %CharSc{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      size: MapEntity.size(entity)
    }
  end

  def render(:cond, entity) do
    %Cond{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      no_attack: not BattleEntity.can_attack(entity),
      no_move: not BattleEntity.can_move(entity),
      speed: MapEntity.speed(entity)
    }
  end

  def render(:eff, %{entity: entity, value: value}) do
    %Eff{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      value: value
    }
  end

  def render(:dir, entity) do
    %Dir{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      direction: MapEntity.direction(entity)
    }
  end
end
