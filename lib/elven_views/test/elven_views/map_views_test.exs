defmodule ElvenViews.MapViewsTest do
  use ViewCase, async: true

  alias ElvenViews.MapViews

  alias ElvenViews.MapPackets.{
    AtPacket,
    CMapPacket,
    MapOutPacket,
    MvPacket
  }

  ## Tests

  describe "render:at" do
    test "returns a packet structure" do
      mock = mock_at()
      render = MapViews.render(:at, mock)

      assert %AtPacket{} = render
      assert render.character_id == mock.character.id
      assert render.map_vnum == mock.character.map_vnum
      assert render.map_x == mock.character.map_x
      assert render.map_y == mock.character.map_y
      assert render.direction == mock.character.direction
      assert render.map_music == mock.map_music
    end
  end

  describe "render:c_map" do
    test "returns a packet structure" do
      mock = mock_c_map()
      render = MapViews.render(:c_map, mock)

      assert %CMapPacket{} = render
      assert render.map_vnum == mock.character.map_vnum
      assert is_boolean(render.is_static_map)
    end
  end

  describe "render:mapout" do
    test "returns a packet structure" do
      mock = mock_mapout()
      render = MapViews.render(:mapout, mock)

      assert %MapOutPacket{} = render
    end
  end

  describe "render:mv" do
    test "returns a packet structure" do
      mock = mock_mv()
      render = MapViews.render(:mv, mock)

      assert %MvPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert render.map_x == mock.entity.map_x
      assert render.map_y == mock.entity.map_y
      assert render.speed == mock.entity.speed
    end
  end

  ## Helpers

  defp mock_at() do
    %{character: character_mock(), map_music: 123}
  end

  defp mock_c_map() do
    %{character: character_mock()}
  end

  defp mock_mapout() do
    %{}
  end

  defp mock_mv() do
    %{entity: character_mock()}
  end
end
