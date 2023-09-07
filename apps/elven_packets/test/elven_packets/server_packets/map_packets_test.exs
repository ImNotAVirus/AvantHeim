defmodule ElvenPackets.Server.MapPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.MapPackets.{At, Cmap, Mapout, Mv}

  ## Tests
  describe "at" do
    test "can be serialized" do
      packet = %At{id: 69, map_vnum: 1, map_x: 2, map_y: 3, direction: :north, map_music: 5}
      assert {"at", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 9
      assert Enum.at(params, 0) == "69"
      assert Enum.at(params, 1) == "1"
      assert Enum.at(params, 2) == "2"
      assert Enum.at(params, 3) == "3"
      assert Enum.at(params, 4) == "0"
      assert Enum.at(params, 5) == "0"
      assert Enum.at(params, 6) == "5"
      assert Enum.at(params, 7) == "-2"
      assert Enum.at(params, 8) == "-1"
    end
  end

  describe "c_map" do
    test "can be serialized" do
      packet = %Cmap{map_vnum: 12, is_static_map: false}
      assert {"c_map", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 3
      assert Enum.at(params, 0) == "0"
      assert Enum.at(params, 1) == "12"
      assert Enum.at(params, 2) == "0"
    end
  end

  describe "mapout" do
    test "can be serialized" do
      assert {"mapout", []} = serialize_packet(%Mapout{})
    end
  end

  describe "mv" do
    test "can be serialized" do
      packet = %Mv{entity_type: :character, entity_id: 2, map_x: 30, map_y: 60, speed: 69}
      assert {"mv", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 5
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "2"
      assert Enum.at(params, 2) == "30"
      assert Enum.at(params, 3) == "60"
      assert Enum.at(params, 4) == "69"
    end
  end
end
