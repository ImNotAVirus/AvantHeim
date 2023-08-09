defmodule ElvenViews.LobbyPackets.OkPacketTest do
  use PacketCase, async: true

  alias ElvenViews.LobbyPackets.OkPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(ok_mock())

      assert is_list(packet)
      assert length(packet) == 1
      assert packet_index(packet, 0) == "OK"
    end
  end

  ## Helpers

  defp ok_mock() do
    %OkPacket{}
  end
end
