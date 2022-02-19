defmodule ElvenViews.PlayerPackets.FsPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.FsPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(fs_mock())

      assert is_list(packet)
      assert length(packet) == 2
      assert packet_index(packet, 0) == "fs"
      assert packet_index(packet, 1) == "1"
    end
  end

  ## Helpers

  defp fs_mock() do
    %FsPacket{faction: :angel}
  end
end
