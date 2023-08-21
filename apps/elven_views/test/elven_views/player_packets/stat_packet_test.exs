defmodule ElvenViews.PlayerPackets.StatPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.StatPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(stat_mock())

      assert is_list(packet)
      assert length(packet) == 7
      assert packet_index(packet, 0) == "stat"
      assert packet_index(packet, 1) == "3"
      assert packet_index(packet, 2) == "4"
      assert packet_index(packet, 3) == "6"
      assert packet_index(packet, 4) == "8"
      assert packet_index(packet, 5) == "0"
      assert packet_index(packet, 6) == "9"
    end
  end

  ## Helpers

  defp stat_mock() do
    %StatPacket{
      hp: 3,
      hp_max: 4,
      mp: 6,
      mp_max: 8,
      option: 9
    }
  end
end
