defmodule ElvenViews.PlayerPackets.RsfiPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.RsfiPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(rsfi_mock())

      assert is_list(packet)
      assert length(packet) == 7
      assert packet_index(packet, 0) == "rsfi"
      assert packet_index(packet, 1) == "3"
      assert packet_index(packet, 2) == "4"
      assert packet_index(packet, 3) == "0"
      assert packet_index(packet, 4) == "0"
      assert packet_index(packet, 5) == "6"
      assert packet_index(packet, 6) == "8"
    end
  end

  ## Helpers

  defp rsfi_mock() do
    %RsfiPacket{
      act: 3,
      act_part: 4,
      ts: 6,
      ts_max: 8
    }
  end
end
