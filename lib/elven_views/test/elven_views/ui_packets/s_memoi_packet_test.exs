defmodule ElvenViews.UIPackets.SMemoiPacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.SMemoiPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(smemoi_mock())

      assert is_list(packet)
      assert length(packet) == 4
      assert packet_index(packet, 0) == "s_memoi"
      assert packet_index(packet, 1) == "4"
      assert packet_index(packet, 2) == "2353"
      assert packet_index(packet, 3) == "0"
    end
  end

  ## Helpers

  defp smemoi_mock() do
    %SMemoiPacket{
      text_color: :green,
      i18n_key: "ThankYouForUsingTheCuarryBank"
    }
  end
end
