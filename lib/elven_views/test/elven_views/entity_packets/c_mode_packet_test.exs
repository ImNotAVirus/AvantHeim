defmodule ElvenViews.EntityPackets.CModePacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.CModePacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(c_mode_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "c_mode"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "111"
      assert packet_index(packet, 3) == "222"
      assert packet_index(packet, 4) == "333"
      assert packet_index(packet, 5) == "444"
      assert packet_index(packet, 6) == "0"
      assert packet_index(packet, 7) == "555"
      assert packet_index(packet, 8) == "666"
    end
  end

  ## Helpers

  defp c_mode_mock() do
    %CModePacket{
      entity_type: :character,
      entity_id: 111,
      morph: 222,
      morph_upgrade: 333,
      morph_design: 444,
      is_arena_winner: false,
      size: 555,
      item_morph: 666
    }
  end
end
