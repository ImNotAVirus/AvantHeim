defmodule ElvenViews.ChatPackets.BnPacketTest do
  use PacketCase, async: true

  alias ElvenViews.ChatPackets.BnPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      mock = bn_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "bn"
      assert packet_index(packet, 1) == mock.id
      assert packet_index(packet, 2) == escape(mock.message)
    end
  end

  ## Helpers

  defp bn_mock() do
    %BnPacket{id: 123, message: "Hello welcome to AvantHeim"}
  end

  defp escape(message) do
    String.replace(message, " ", "^")
  end
end
