defmodule ElvenPackets.Views.MapViews do
  @moduledoc """
  TODO: ElvenPackets.Views.MapViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [required_param: 2]

  alias ElvenPackets.Server.MapPackets.{At, CMap, Mapout, Mv}
  alias GameService.PlayerBundle
  alias GameService.EntityComponents.PositionComponent

  @impl true
  def render(:at, args) do
    entity = required_param(args, :entity)
    map_music = required_param(args, :map_music)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError, "at can only be called on players currently, got: #{inspect(entity)}"
    end

    %At{
      entity_id: GameService.entity_id(entity),
      map_vnum: PlayerBundle.map_id(entity),
      map_x: PlayerBundle.map_x(entity),
      map_y: PlayerBundle.map_y(entity),
      direction: PlayerBundle.direction(entity),
      map_music: map_music
    }
  end

  def render(:c_map, args) do
    entity = required_param(args, :entity)

    if entity.__struct__ != PlayerBundle do
      raise ArgumentError,
            "c_map can only be called on players currently, got: #{inspect(entity)}"
    end

    %CMap{
      map_vnum: PlayerBundle.map_id(entity),
      is_static_map: not PositionComponent.map_instance?(entity.position)
    }
  end

  def render(:mapout, _args) do
    %Mapout{}
  end

  def render(:mv, args) do
    entity = required_param(args, :entity)

    %Mv{
      entity_type: GameService.entity_type(entity),
      entity_id: GameService.entity_id(entity),
      map_x: PlayerBundle.map_x(entity),
      map_y: PlayerBundle.map_y(entity),
      speed: PlayerBundle.speed(entity)
    }
  end
end
