defmodule ElvenViews.UIPackets.GoldPacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.GoldPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(gold_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "gold"
      assert packet_index(packet, 1) == "1000"
      assert packet_index(packet, 2) == "2000"
    end
  end

  ## Helpers

  defp gold_mock() do
    %GoldPacket{
      gold: 1000,
      bank_gold: 2000
    }
  end
end
