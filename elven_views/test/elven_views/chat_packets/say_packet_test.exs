defmodule ElvenViews.ChatPackets.SayPacketTest do
  use PacketCase, async: true

  require ElvenEnums.EntityEnums
  require ElvenViews.ChatPackets.SayEnums

  alias ElvenEnums.EntityEnums
  alias ElvenViews.ChatPackets.SayPacket
  alias ElvenViews.ChatPackets.SayEnums

  ## Tests

  describe "serialize/2" do
    test "can serialize entity types" do
      mock = say_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "say"
      assert packet_index(packet, 1) == EntityEnums.entity_type(mock.entity_type, :value)
      assert packet_index(packet, 2) == mock.entity_id
      assert packet_index(packet, 3) == SayEnums.color_type(mock.color, :value)
      assert packet_index(packet, 4) == mock.message
    end
  end

  ## Helpers

  defp say_mock() do
    %SayPacket{
      entity_type: :character,
      entity_id: 1,
      color: :default,
      message: "This is a message for the SayPacket"
    }
  end
end
