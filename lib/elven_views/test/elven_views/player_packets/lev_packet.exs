defmodule ElvenViews.PlayerPackets.LevPacketTest do
  use PacketCase, async: true

  alias ElvenViews.PlayerPackets.LevPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      packet = structure_to_iolist(lev_mock())

      assert is_list(packet)
      assert length(packet) == 13
      assert packet_index(packet, 0) == "lev"
      assert packet_index(packet, 1) == "17"
      assert packet_index(packet, 2) == "600"
      assert packet_index(packet, 3) == "40"
      assert packet_index(packet, 4) == "3015"
      assert packet_index(packet, 5) == "47080"
      assert packet_index(packet, 6) == "79400"
      assert packet_index(packet, 7) == "78000"
      assert packet_index(packet, 8) == "12"
      assert packet_index(packet, 9) == "504579"
      assert packet_index(packet, 10) == "60"
      assert packet_index(packet, 11) == "454600"
      assert packet_index(packet, 12) == "0"
    end
  end

  ## Helpers

  defp lev_mock() do
    %LevPacket{
      level: 17,
      level_xp: 600,
      job_level: 40,
      job_level_xp: 3015,
      level_xp_max: 47080,
      job_level_xp_max: 79400,
      reputation: 78000,
      cp: 12,
      hero_level_xp: 504_579,
      hero_level: 60,
      hero_level_xp_max: 454_600
    }
  end
end
