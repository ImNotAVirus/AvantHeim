defmodule ChannelService.Endpoint.MapViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.Entity.EntityPosition
  alias ElvenCaching.Entity.Character

  alias ChannelService.Endpoint.MapPackets.{
    At,
    CMap,
    MapOut,
    Mv
  }

  ## Public API

  @spec render(atom, any) :: any
  def render(:at, %Character{} = character) do
    %EntityPosition{
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y
    } = MapEntity.position(character)

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
    %EntityPosition{
      map_vnum: map_vnum,
      is_instance: is_instance
    } = MapEntity.position(character)

    %CMap{
      map_vnum: map_vnum,
      is_static_map: not is_instance
    }
  end

  def render(:mapout, %Character{}) do
    %MapOut{}
  end

  def render(:mv, entity) do
    %EntityPosition{
      map_x: map_x,
      map_y: map_y
    } = MapEntity.position(entity)

    %Mv{
      entity_type: Entity.type(entity),
      entity_id: Entity.id(entity),
      map_x: map_x,
      map_y: map_y,
      speed: MapEntity.speed(entity)
    }
  end
end