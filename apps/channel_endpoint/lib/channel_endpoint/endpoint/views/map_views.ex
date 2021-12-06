defmodule ChannelEndpoint.Endpoint.MapViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Position
  alias CachingService.Player.Character
  alias CachingService.Map.Monster

  alias ChannelEndpoint.Endpoint.MapPackets.{
    At,
    CMap,
    MapOut,
    Mv,
    Rest
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:at, %Character{} = character) do
    %Position{
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y
    } = Character.get_position(character)

    %At{
      character_id: character.id,
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y,
      direction: FakeData.direction(character_id: character.id),
      map_music: FakeData.map_music(character_id: character.id)
    }
  end

  def render(:c_map, %Character{} = character) do
    %Position{
      map_vnum: map_vnum,
      is_instance: is_instance
    } = Character.get_position(character)

    %CMap{
      map_vnum: map_vnum,
      is_static_map: not is_instance
    }
  end

  def render(:mapout, %Character{}) do
    %MapOut{}
  end

  def render(:mv, %Character{} = character) do
    %Position{
      map_x: map_x,
      map_y: map_y
    } = Character.get_position(character)

    %Mv{
      entity_type: :character,
      entity_id: character.id,
      map_x: map_x,
      map_y: map_y,
      speed: character.speed
    }
  end

  def render(:mv, %Monster{} = monster) do
    %Mv{
      entity_type: :monster,
      entity_id: monster.id,
      map_x: monster.map_x,
      map_y: monster.map_y,
      speed: monster.speed
    }
  end

  def render(:rest, %Monster{} = monster) do
    %Rest{
      entity_type: :monster,
      entity_id: monster.id,
      is_sitting: monster.is_sitting
    }
  end
end
