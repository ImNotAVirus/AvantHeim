defmodule ElvenViews.EntityPackets.CModePacketTest do
  use PacketCase, async: true

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias ElvenViews.EntityPackets.CModePacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      mock = c_mode_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "c_mode"
      assert packet_index(packet, 1) == EntityEnums.entity_type(mock.entity_type, :value)
      assert packet_index(packet, 2) == mock.entity_id
      assert packet_index(packet, 3) == mock.morph
      assert packet_index(packet, 4) == mock.morph_upgrade
      assert packet_index(packet, 5) == mock.morph_design
      assert packet_index(packet, 6) == mock.is_arena_winner
      assert packet_index(packet, 7) == mock.size
      assert packet_index(packet, 8) == mock.item_morph
    end
  end

  ## Helpers

  defp c_mode_mock() do
    %CModePacket{
      entity_type: :character,
      entity_id: 1,
      morph: 11,
      morph_upgrade: 15,
      morph_design: 10,
      is_arena_winner: false,
      size: 13,
      item_morph: 1
    }
  end
end
