defmodule ElvenViews.UIPackets.GbPacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.GbPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(gb_mock())

      assert is_list(packet)
      assert length(packet) == 6
      assert packet_index(packet, 0) == "gb"
      assert packet_index(packet, 1) == "0"
      assert packet_index(packet, 2) == "1000"
      assert packet_index(packet, 3) == "2000"
      assert packet_index(packet, 4) == "31"
      assert packet_index(packet, 5) == "50"
    end
  end

  ## Helpers

  defp gb_mock() do
    %GbPacket{
      action_type: :open_from_savings_book,
      bank_gold: 1000,
      gold: 2000,
      bank_rank: 31,
      bank_tax: 50
    }
  end
end
