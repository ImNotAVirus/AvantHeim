defmodule ElvenViews.UIPackets.SMemoi2PacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.SMemoi2Packet

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(smemoi2_mock())

      assert is_list(packet)
      assert length(packet) == 5
      assert packet_index(packet, 0) == "s_memoi2"
      assert packet_index(packet, 1) == "4"
      assert packet_index(packet, 2) == "2154"
      assert packet_index(packet, 3) == "5000"
      assert packet_index(packet, 4) == "6999"
    end
  end

  ## Helpers

  defp smemoi2_mock() do
    %SMemoi2Packet{
      text_color: :green,
      i18n_key: "LolaLopears",
      bank_gold: 5000,
      gold: 6999
    }
  end
end
