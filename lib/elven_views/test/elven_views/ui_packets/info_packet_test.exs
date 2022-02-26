defmodule ElvenViews.UIPackets.InfoPacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.InfoPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(info_mock())

      assert is_list(packet)
      assert length(packet) == 2
      assert packet_index(packet, 0) == "info"
      assert packet_index(packet, 1) == "This is Info Packet"
    end
  end

  ## Helpers

  defp info_mock() do
    %InfoPacket{
      message: "This is Info Packet"
    }
  end
end
