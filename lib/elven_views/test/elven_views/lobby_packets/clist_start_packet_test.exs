defmodule ElvenViews.LobbyPackets.ClistStartPacketTest do
  use PacketCase, async: true

  alias ElvenViews.LobbyPackets.ClistStartPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(clist_start_mock())

      assert is_list(packet)
      assert length(packet) == 2
      assert packet_index(packet, 0) == "clist_start"
      assert packet_index(packet, 1) == "0"
    end
  end

  ## Helpers

  defp clist_start_mock() do
    %ClistStartPacket{}
  end
end
