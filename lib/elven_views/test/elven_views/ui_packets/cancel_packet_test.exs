defmodule ElvenViews.UIPackets.CancelPacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.CancelPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(cancel_mock())

      assert is_list(packet)
      assert length(packet) == 4
      assert packet_index(packet, 0) == "cancel"
      assert packet_index(packet, 1) == "0"
      assert packet_index(packet, 2) == "132"
      assert packet_index(packet, 3) == "-1"
    end
  end

  ## Helpers

  defp cancel_mock() do
    %CancelPacket{
      cancel_type: :skill,
      entity_id: 132
    }
  end
end
