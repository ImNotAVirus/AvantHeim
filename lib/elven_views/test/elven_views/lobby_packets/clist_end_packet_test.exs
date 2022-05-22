defmodule ElvenViews.LobbyPackets.ClistEndPacketTest do
  use PacketCase, async: true

  alias ElvenViews.LobbyPackets.ClistEndPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(clist_end_mock())

      assert is_list(packet)
      assert length(packet) == 1
      assert packet_index(packet, 0) == "clist_end"
    end
  end

  ## Helpers

  defp clist_end_mock() do
    %ClistEndPacket{}
  end
end
