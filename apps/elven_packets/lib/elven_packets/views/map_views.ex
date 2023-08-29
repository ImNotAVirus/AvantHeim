defmodule ElvenPackets.Views.MapViews do
  @moduledoc """
  TODO: ElvenPackets.Views.MapViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 3, required_param: 2]

  alias ElvenCaching.Entity.EntityPosition
  alias ElvenPackets.Server.MapPackets.{At, Cmap, Mapout, Mv}

  @impl true
  def render(:at, args) do
    character = required_param(args, :character)
    map_music = required_param(args, :map_music)

    %EntityPosition{
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y
    } = MapEntity.position(character)

    %At{
      id: Entity.id(character),
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y,
      direction: MapEntity.direction(character),
      map_music: map_music
    }
  end

  def render(:c_map, args) do
    character = required_param(args, :character)

    %EntityPosition{
      map_vnum: map_vnum,
      is_instance: is_instance
    } = MapEntity.position(character)

    %Cmap{
      map_vnum: map_vnum,
      is_static_map: not is_instance
    }
  end

  def render(:mapout, _args) do
    %Mapout{}
  end

  def render(:mv, args) do
    entity = required_param(args, :entity)

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
