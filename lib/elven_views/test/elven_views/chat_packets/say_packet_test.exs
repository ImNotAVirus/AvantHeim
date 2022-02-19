defmodule ElvenViews.ChatPackets.SayPacketTest do
  use PacketCase, async: true

  alias ElvenViews.ChatPackets.SayPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize entity types" do
      packet = structure_to_iolist(say_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "say"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "123"
      assert packet_index(packet, 3) == "10"
      assert packet_index(packet, 4) == "This is a message for the SayPacket"
    end
  end

  ## Helpers

  defp say_mock() do
    %SayPacket{
      entity_type: :character,
      entity_id: 123,
      color: :special_gold,
      message: "This is a message for the SayPacket"
    }
  end
end
