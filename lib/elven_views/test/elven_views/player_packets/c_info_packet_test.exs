defmodule ElvenViews.PlayerPackets.CInfoPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.CInfoPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(c_info_mock())

      assert is_list(packet)
      assert length(packet) == 19
      assert packet_index(packet, 0) == "c_info"
      assert packet_index(packet, 1) == "4615"
      assert packet_index(packet, 2) == "DarkyZ"
      assert packet_index(packet, 3) == "221"
      assert packet_index(packet, 4) == "331"
      assert packet_index(packet, 5) == "2"
      assert packet_index(packet, 6) == "AvantHeim"
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
      assert packet_index(packet, 18) == "1"
    end
  end

  ## Helpers

  defp c_info_mock() do
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
      is_arena_winner: true
    }
  end
end
