defmodule ElvenViews.VisibilityPackets.OutPacketTest do
  use PacketCase, async: true

  alias ElvenViews.VisibilityPackets.OutPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(out_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "out"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "485"
    end
  end

  ## Helpers

  defp out_mock() do
    %OutPacket{
      entity_type: :character,
      entity_id: 485
    }
  end
end
