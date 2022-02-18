defmodule ElvenViews.EntityPackets.DirPacketTest do
  use PacketCase, async: true

  require ElvenEnums.EntityEnums

  alias ElvenEnums.EntityEnums
  alias ElvenViews.EntityPackets.DirPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      mock = dir_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "dir"
      assert packet_index(packet, 1) == EntityEnums.entity_type(mock.entity_type, :value)
      assert packet_index(packet, 2) == mock.entity_id
      assert packet_index(packet, 3) == EntityEnums.direction_type(mock.direction_type, :value)
    end
  end

  ## Helpers

  defp dir_mock() do
    %DirPacket{
      entity_type: :character,
      entity_id: 1,
      direction_type: :north
    }
  end
end
