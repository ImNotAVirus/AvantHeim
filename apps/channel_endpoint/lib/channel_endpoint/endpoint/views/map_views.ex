defmodule ChannelEndpoint.Endpoint.MapViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Position
  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.MapPackets.{
    At,
    CMap,
    Mv
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
end
