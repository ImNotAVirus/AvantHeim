defmodule ElvenViews.ShopPackets.BuyPacketTest do
  use PacketCase, async: true

  alias ElvenViews.ShopPackets.BuyPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(buy_mock())

      assert is_list(packet)
      assert length(packet) == 5
      assert packet_index(packet, 0) == "buy"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "748"
      assert packet_index(packet, 3) == "24"
      assert packet_index(packet, 4) == "12"
    end
  end

  ## Helpers

  defp buy_mock() do
    %BuyPacket{
      shop_type: :character_shop,
      entity_id: 748,
      slot: 24,
      amount: 12
    }
  end
end
