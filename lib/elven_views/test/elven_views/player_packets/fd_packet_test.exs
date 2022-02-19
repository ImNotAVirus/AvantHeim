defmodule ElvenViews.PlayerPackets.FdPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.FdPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(fd_mock())

      assert is_list(packet)
      assert length(packet) == 5
      assert packet_index(packet, 0) == "fd"
      assert packet_index(packet, 1) == "560"
      assert packet_index(packet, 2) == "6"
      assert packet_index(packet, 3) == "100"
      assert packet_index(packet, 4) == "3"
    end
  end

  ## Helpers

  defp fd_mock() do
    %FdPacket{
      reputation: 560,
      reputation_icon_id: 6,
      dignity: 100,
      dignity_icon_id: 3
    }
  end
end
