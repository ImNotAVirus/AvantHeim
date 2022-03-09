defmodule ElvenViews.UIPackets.SMemoi2PacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.SMemoi2Packet
  alias ElvenViews.SubPackets.I18nSubPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(smemoi2_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "s_memoi2"
      assert packet_index(packet, 1) == "4"
      assert packet_index(packet, 2) == "2345 3 5,000 6,999,000 0"
    end
  end

  ## Helpers

  defp smemoi2_mock() do
    %SMemoi2Packet{
      text_color: :green,
      i18n_packet: %I18nSubPacket{
        key: "BalanceBank",
        args: ["5,000", "6,999,000"]
      }
    }
  end
end
