defmodule ElvenViews.EntityPackets.DirPacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.DirPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(dir_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "dir"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "22"
      assert packet_index(packet, 3) == "0"
    end
  end

  ## Helpers

  defp dir_mock() do
    %DirPacket{
      entity_type: :character,
      entity_id: 22,
      direction_type: :north
    }
  end
end
