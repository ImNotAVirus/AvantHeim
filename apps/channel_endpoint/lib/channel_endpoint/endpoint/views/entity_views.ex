defmodule ChannelEndpoint.Endpoint.EntityViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Map.Monster
  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.EntityPackets.{
    CharSc,
    CMode,
    Cond,
    Eff,
    St,
    Dir
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:c_mode, %Character{} = character) do
    %CMode{
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

  def render(:st, %Character{} = character) do
    %St{
      entity_type: :character,
      entity_id: character.id,
      level: character.level,
      hero_level: character.hero_level,
      hp: FakeData.hp(character_id: character.id),
      hp_max: FakeData.hp_max(character_id: character.id),
      mp: FakeData.mp(character_id: character.id),
      mp_max: FakeData.mp_max(character_id: character.id),
      buffs: []
    }
  end

  def render(:st, %Monster{} = monster) do
    %St{
      entity_type: :monster,
      entity_id: monster.id,
      level: monster.level,
      hp: monster.hp,
      hp_max: monster.hp_max,
      mp: monster.mp,
      mp_max: monster.mp_max,
      buffs: []
    }
  end

  def render(:char_sc, %Character{} = character) do
    %CharSc{
      entity_type: :character,
      entity_id: character.id,
      size: FakeData.size(character_id: character.id)
    }
  end

  def render(:cond, %Character{} = character) do
    %Cond{
      entity_type: :character,
      entity_id: character.id,
      no_attack: FakeData.no_attack(character_id: character.id),
      no_move: FakeData.no_move(character_id: character.id),
      speed: character.speed
    }
  end

  # TODO : Improve that to support pnj | mobs | mates
  def render(:eff, %{entity: %Character{id: id}, value: value}) do
    %Eff{
      entity_type: :character,
      entity_id: id,
      value: value
    }
  end

  def render(:dir, %Character{} = character) do
    %Dir{
      entity_type: :character,
      entity_id: character.id,
      direction: character.direction
    }
  end
end
