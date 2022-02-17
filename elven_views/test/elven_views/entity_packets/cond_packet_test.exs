defmodule ElvenViews.EntityPackets.CondPacketTest do
  use PacketCase, async: true

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias ElvenViews.EntityPackets.CondPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      mock = cond_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "cond"
      assert packet_index(packet, 1) == EntityEnums.entity_type(mock.entity_type, :value)
      assert packet_index(packet, 2) == mock.entity_id
      assert packet_index(packet, 3) == mock.no_attack
      assert packet_index(packet, 4) == mock.no_move
      assert packet_index(packet, 5) == mock.speed
    end
  end

  ## Helpers

  defp cond_mock() do
    %CondPacket{
      entity_type: :character,
      entity_id: 1,
      no_attack: false,
      no_move: false,
      speed: 10
    }
  end
end
