defmodule ElvenViews.MapPackets.CMapPacketTest do
  use PacketCase, async: true

  alias ElvenViews.MapPackets.CMapPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(c_map_mock())

      assert is_list(packet)
      assert length(packet) == 4
      assert packet_index(packet, 0) == "c_map"
      assert packet_index(packet, 1) == "0"
      assert packet_index(packet, 2) == "15"
      assert packet_index(packet, 3) == "1"
    end
  end

  ## Helpers

  defp c_map_mock() do
    %CMapPacket{
      map_vnum: 15,
      is_static_map: true
    }
  end
end
