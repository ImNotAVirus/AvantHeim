defmodule ElvenPackets.Server.EntityPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.EntityPackets.{CMode, CharSc, Cond, Dir, Eff, St}

  ## Tests

  describe "c_mode" do
    test "can be serialized" do
      assert {"c_mode", params} = serialize_packet(c_mode_mock())
      assert is_list(params)
      assert length(params) == 8
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "111"
      assert Enum.at(params, 2) == "19"
      assert Enum.at(params, 3) == "333"
      assert Enum.at(params, 4) == "7"
      assert Enum.at(params, 5) == "0"
      assert Enum.at(params, 6) == "555"
      assert Enum.at(params, 7) == "666"
    end
  end

  describe "char_sc" do
    test "can be serialized" do
      packet = %CharSc{entity_type: :character, entity_id: 11, size: 22}
      assert {"char_sc", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 3
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "11"
      assert Enum.at(params, 2) == "22"
    end
  end

  describe "cond" do
    test "can be serialized" do
      packet = %Cond{entity_type: :monster, entity_id: 22, no_attack: true, no_move: false, speed: 33}
      assert {"cond", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(packet) == 5
      assert Enum.at(params, 0) == "3"
      assert Enum.at(params, 1) == "22"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "0"
      assert Enum.at(params, 4) == "33"
    end
  end

  describe "dir" do
    test "can be serialized" do
      packet = %Dir{entity_type: :character, entity_id: 22, direction: :north}
      assert {"dir", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(packet) == 3
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "22"
      assert Enum.at(params, 2) == "0"
    end
  end

  describe "eff" do
    test "can be serialized" do
      packet = %Eff{entity_type: :character, entity_id: 11, value: 22}
      assert {"eff", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(packet) == 3
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "11"
      assert Enum.at(params, 2) == "22"
    end
  end

  describe "st" do
    test "can serialize a packet structure" do
      assert {"st", params} = serialize_packet(st_mock())
      assert is_list(params)
      assert length(params) == 8
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "11"
      assert Enum.at(params, 2) == "22"
      assert Enum.at(params, 3) == "33"
      assert Enum.at(params, 4) == "44"
      assert Enum.at(params, 5) == "55"
      assert Enum.at(params, 6) == "66"
      assert Enum.at(params, 7) == "77"
    end

    test "can serialize a packet with buffs" do
      # Single buff
      attrs = %{buffs: [111]}
      assert {"st", params} = serialize_packet(st_mock(attrs))

      assert length(packet) == 9
      assert Enum.at(params, 8) == "111"

      # Multiple buffs
      attrs = %{buffs: [111, 222, 333]}
      assert {"st", params} = serialize_packet(st_mock(attrs))

      assert length(packet) == 9
      assert Enum.at(params, 8) == "111 222 333"
    end
  end

  ## Helpers

  defp c_mode_mock() do
    %CMode{
      entity_type: :character,
      entity_id: 111,
      morph: :volcano,
      morph_upgrade: 333,
      morph_design: :archangel_wings,
      is_arena_winner: false,
      size: 555,
      item_morph: 666
    }
  end

  defp st_mock(attrs \\ %{}) do
    Map.merge(
      %St{
        entity_type: :character,
        entity_id: 11,
        level: 22,
        hero_level: 33,
        hp_percent: 44,
        mp_percent: 55,
        hp: 66,
        mp: 77,
        buffs: []
      },
      attrs
    )
  end

end
