defmodule ElvenViews.EntityPackets.CharScPacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.CharScPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(char_sc_mock())

      assert is_list(packet)
      assert length(packet) == 4
      assert packet_index(packet, 0) == "char_sc"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "11"
      assert packet_index(packet, 3) == "22"
    end
  end

  ## Helpers

  defp char_sc_mock() do
    %CharScPacket{
      entity_type: :character,
      entity_id: 11,
      size: 22
    }
  end
end
