defmodule ElvenViews.MapPackets.AtPacketTest do
  use PacketCase, async: true

  alias ElvenViews.MapPackets.AtPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(at_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "at"
      assert packet_index(packet, 1) == "4574"
      assert packet_index(packet, 2) == "1"
      assert packet_index(packet, 3) == "79"
      assert packet_index(packet, 4) == "116"
      assert packet_index(packet, 5) == "0"
      assert packet_index(packet, 6) == "2770"
    end
  end
  
  ## Helpers

  defp at_mock() do
    %AtPacket{
      character_id: 4574,
      map_vnum: 1,
      map_x: 79,
      map_y: 116,
      direction: :north,
      map_music: 2770
    }
  end
end
