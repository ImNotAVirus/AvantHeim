defmodule GameService.GameConfigTest do
  # FIXME: Rewrite this module
  # Currently systems and game service are automatically spawned
  # Need to rewrite using `test --no-start`
  use ExUnit.Case, async: true

  alias GameService.GameConfig
  alias GameService.Structures.PortalStructure

  ## Setup

  # setup_all do
  #   GameConfig.init()
  #   on_exit(fn -> GameConfig.clean() end)
  # end

  ## Tests

  test "load map info" do
    assert GameConfig.maps_info() == [{1, 1, 1, 0, true}, {2, 2, 2, 1, false}]
    assert GameConfig.static_map_info_ids() == [1]
  end

  test "load map monsters" do
    assert GameConfig.map_monsters(1) == [
             %{id: 6300, map_id: 1, vnum: 333, map_x: 30, map_y: 165},
             %{id: 6301, map_id: 1, vnum: 333, map_x: 24, map_y: 157},
             %{id: 6302, map_id: 1, vnum: 333, map_x: 22, map_y: 158}
           ]
  end

  test "load map portals" do
    assert GameConfig.map_portals(1) == [
             %PortalStructure{
               source_map_id: 1,
               source_map_x: 159,
               source_map_y: 6,
               destination_map_id: 2550,
               destination_map_ref: 2550,
               destination_map_x: 20,
               destination_map_y: 38,
               type: -1
             },
             %PortalStructure{
               source_map_id: 1,
               source_map_x: 80,
               source_map_y: 66,
               destination_map_id: 147,
               destination_map_ref: 147,
               destination_map_x: 18,
               destination_map_y: 2,
               type: 20
             },
             %PortalStructure{
               source_map_id: 1,
               source_map_x: 117,
               source_map_y: 177,
               destination_map_id: 145,
               destination_map_ref: 145,
               destination_map_x: 59,
               destination_map_y: 2,
               type: -1
             },
             %PortalStructure{
               source_map_id: 1,
               source_map_x: 79,
               source_map_y: 2,
               destination_map_id: 2,
               destination_map_ref: 2,
               destination_map_x: 140,
               destination_map_y: 148,
               type: -1
             }
           ]
  end
end
