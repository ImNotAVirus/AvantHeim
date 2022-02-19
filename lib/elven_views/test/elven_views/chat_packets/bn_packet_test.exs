defmodule ElvenViews.ChatPackets.BnPacketTest do
  use PacketCase, async: true

  alias ElvenViews.ChatPackets.BnPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(bn_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "bn"
      assert packet_index(packet, 1) == "123"
      assert packet_index(packet, 2) == "Hello^welcome^to^AvantHeim"
    end
  end

  ## Helpers

  defp bn_mock() do
    %BnPacket{id: 123, message: "Hello welcome to AvantHeim"}
  end
end
