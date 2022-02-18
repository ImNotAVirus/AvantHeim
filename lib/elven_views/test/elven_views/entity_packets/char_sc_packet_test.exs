defmodule ElvenViews.EntityPackets.CharScPacketTest do
  use PacketCase, async: true

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias ElvenViews.EntityPackets.CharScPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      mock = char_sc_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "char_sc"
      assert packet_index(packet, 1) == EntityEnums.entity_type(mock.entity_type, :value)
      assert packet_index(packet, 2) == mock.entity_id
      assert packet_index(packet, 3) == mock.size
    end
  end

  ## Helpers

  defp char_sc_mock() do
    %CharScPacket{
      entity_type: :character,
      entity_id: 1,
      size: 13
    }
  end
end
