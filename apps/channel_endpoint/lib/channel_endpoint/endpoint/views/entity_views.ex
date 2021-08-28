defmodule ChannelEndpoint.Endpoint.EntityViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.EntityPackets.{
    CharSc,
    CMode,
    Cond,
    Eff
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
end
