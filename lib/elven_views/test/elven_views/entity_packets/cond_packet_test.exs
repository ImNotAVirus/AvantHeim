defmodule ElvenViews.EntityPackets.CondPacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.CondPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(cond_mock())

      assert is_list(packet)
      assert packet_index(packet, 0) == "cond"
      assert packet_index(packet, 1) == "3"
      assert packet_index(packet, 2) == "22"
      assert packet_index(packet, 3) == "1"
      assert packet_index(packet, 4) == "0"
      assert packet_index(packet, 5) == "33"
    end
  end

  ## Helpers

  defp cond_mock() do
    %CondPacket{
      entity_type: :monster,
      entity_id: 22,
      no_attack: true,
      no_move: false,
      speed: 33
    }
  end
end
