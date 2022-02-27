defmodule ElvenViews.EntityPackets.EffPacketTest do
  use PacketCase, async: true

  alias ElvenViews.EntityPackets.EffPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(eff_mock())

      assert is_list(packet)
      assert length(packet) == 4
      assert packet_index(packet, 0) == "eff"
      assert packet_index(packet, 1) == "1"
      assert packet_index(packet, 2) == "11"
      assert packet_index(packet, 3) == "22"
    end
  end

  ## Helpers

  defp eff_mock() do
    %EffPacket{
      entity_type: :character,
      entity_id: 11,
      value: 22
    }
  end
end
