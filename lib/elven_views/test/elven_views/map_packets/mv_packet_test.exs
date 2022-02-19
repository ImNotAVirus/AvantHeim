defmodule ElvenViews.MapPackets.MvPacketTest do
  use PacketCase, async: true

  alias ElvenViews.MapPackets.MvPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(mv_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "mv"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "1519"
      assert packet_index(packet, 3) == "93"
      assert packet_index(packet, 4) == "54"
      assert packet_index(packet, 5) == "20"
    end
  end

  ## Helpers

  defp mv_mock() do
    %MvPacket{
      entity_type: :character,
      entity_id: 1519,
      map_x: 93,
      map_y: 54,
      speed: 20
    }
  end
end
