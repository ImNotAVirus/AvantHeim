defmodule ElvenViews.MapPackets.MapOutPacketTest do
  use PacketCase, async: true

  alias ElvenViews.MapPackets.MapOutPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(mapout_mock())

      assert is_list(packet)
      assert length(packet) == 1
      assert packet_index(packet, 0) == "mapout"
    end
  end

  ## Helpers

  defp mapout_mock() do
    %MapOutPacket{}
  end
end
