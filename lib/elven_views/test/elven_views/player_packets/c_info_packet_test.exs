defmodule ElvenViews.PlayerPackets.CInfoPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.CInfoPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(c_info_mock())

      assert is_list(packet)
      assert length(packet) == 20
      assert packet_index(packet, 0) == "c_info"
      assert packet_index(packet, 1) == "DarkyZ"
      assert packet_index(packet, 2) == "-"
      assert packet_index(packet, 3) == "221"
      assert packet_index(packet, 4) == "331.916"
      assert packet_index(packet, 5) == "AvantHeim"
      assert packet_index(packet, 6) == "4615"
      assert packet_index(packet, 7) == "10"
      assert packet_index(packet, 8) == "0"
      assert packet_index(packet, 9) == "3"
      assert packet_index(packet, 10) == "5"
      assert packet_index(packet, 11) == "4"
      assert packet_index(packet, 12) == "80"
      assert packet_index(packet, 13) == "100"
      assert packet_index(packet, 14) == "1024"
      assert packet_index(packet, 15) == "0"
      assert packet_index(packet, 16) == "12"
      assert packet_index(packet, 17) == "15"
      assert packet_index(packet, 18) == "16"
      assert packet_index(packet, 19) == "1"
    end

    test "can serialize when no family is given" do
      family_attrs = %{
        family_id: -1,
        family_rank: :member,
        family_name: nil,
        family_level: 0
      }

      packet = structure_to_iolist(c_info_mock(family_attrs))

      assert packet_index(packet, 4) == "-1"
      assert packet_index(packet, 5) == "-"
      assert packet_index(packet, 16) == "0"
    end
  end

  ## Helpers

  defp c_info_mock(attrs \\ %{}) do
    Map.merge(
      %CInfoPacket{
        character_id: 4615,
        name: "DarkyZ",
        group_id: 221,
        family_id: 331,
        family_rank: :deputy,
        family_name: "AvantHeim",
        name_color_id: 10,
        gender: :male,
        hair_style: :hair_style_d,
        hair_color: :brown,
        class: :martial_artist,
        reputation_icon_id: 80,
        compliment: 100,
        morph: 1024,
        is_invisible: false,
        family_level: 12,
        morph_upgrade: 15,
        morph_design: 16,
        is_arena_winner: true
      },
      attrs
    )
  end
end
