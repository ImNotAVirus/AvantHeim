defmodule ChannelEndpoint.Endpoint.MapViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character

  alias ChannelEndpoint.Endpoint.MapPackets.{
    At,
    CMap
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:at, %Character{} = character) do
    %FakeData.Position{
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y
    } = FakeData.get_position(character_id: character.id)

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
    %FakeData.Position{
      map_vnum: map_vnum,
      is_instance: is_instance
    } = FakeData.get_position(character_id: character.id)

    %CMap{
      map_vnum: map_vnum,
      is_static_map: not is_instance
    }
  end
end
