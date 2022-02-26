defmodule ElvenViews.UIPackets.ScenePacketTest do
  use PacketCase, async: true

  alias ElvenViews.UIPackets.ScenePacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(scene_mock())

      assert is_list(packet)
      assert length(packet) == 3
      assert packet_index(packet, 0) == "scene"
      assert packet_index(packet, 1) == "18"
      assert packet_index(packet, 2) == "1"
    end
  end

  ## Helpers

  defp scene_mock() do
    %ScenePacket{
      scene_id: 18,
      cancellable: true
    }
  end
end
