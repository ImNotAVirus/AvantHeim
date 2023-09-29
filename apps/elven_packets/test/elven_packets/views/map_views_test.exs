defmodule ElvenPackets.Views.MapViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.MapViews
  alias ElvenPackets.Server.MapPackets.{At, CMap, Mapout, Mv}

  ## Tests

  describe "at" do
    test "default serialization for players" do
      args = %{entity: new_player(), map_music: 9}
      packet = MapViews.render(:at, args)

      assert %At{} = packet
      assert packet.entity_id == args.entity.id
      assert packet.map_vnum == 123
      assert packet.map_x == 12
      assert packet.map_y == 34
      assert packet.direction == :south
      assert packet.map_music == args.map_music
    end
  end

  describe "c_map" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = MapViews.render(:c_map, args)

      assert %CMap{} = packet
      assert packet.map_vnum == 123
      assert packet.is_static_map == false
    end
  end

  describe "mapout" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = MapViews.render(:mapout, args)

      assert %Mapout{} = packet
    end
  end

  describe "mv" do
    test "default serialization for players" do
      entity = new_player()

      args = %{
        entity_type: :player,
        entity_id: entity.id,
        map_x: entity.position.map_x,
        map_y: entity.position.map_y,
        speed: entity.speed.value
      }

      packet = MapViews.render(:mv, args)

      assert %Mv{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity_id
      assert packet.map_x == 12
      assert packet.map_y == 34
      assert packet.speed == 40
    end
  end
end
