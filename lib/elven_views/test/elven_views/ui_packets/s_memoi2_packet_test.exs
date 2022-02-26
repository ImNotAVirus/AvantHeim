defmodule ElvenViews.UIPackets.SMemoi2PacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.SMemoi2Packet

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(smemoi2_mock())

      assert is_list(packet)
      assert length(packet) == 7
      assert packet_index(packet, 0) == "s_memoi2"
      assert packet_index(packet, 1) == "4"
      assert packet_index(packet, 2) == "2154"
      assert packet_index(packet, 3) == "3"
      assert packet_index(packet, 4) == "5,000"
      assert packet_index(packet, 5) == "6,999,000"
      assert packet_index(packet, 6) == "0"
    end
  end

  ## Helpers

  defp smemoi2_mock() do
    %SMemoi2Packet{
      text_color: :green,
      i18n_key: "LolaLopears",
      bank_gold: 5000,
      gold: 6_999_000
    }
  end
end
