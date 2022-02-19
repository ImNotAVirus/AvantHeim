defmodule ElvenViews.LoginPackets.NsTeSTPacketTest do
  use PacketCase, async: true

  alias ElvenViews.LoginPackets.NsTeSTPacket
  alias ElvenViews.LoginPackets.NsTeST.Channel

  ## Tests

  describe "serialize/2" do
    test "with empty channel list" do
      mock = nstest_mock([])
      packet = structure_to_iolist(mock)

      assert is_list(packet)
      assert length(packet) == 15
      assert packet_index(packet, 0) == "NsTeST"
      assert packet_index(packet, 1) == "0"
      assert packet_index(packet, 2) == "admin"
      assert packet_index(packet, 3) == "2"
      assert packet_index(packet, 4) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 5) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 6) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 7) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 8) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 9) == "-99 0 -99 0 -99 0 -99 0"
      assert packet_index(packet, 10) == unused_server()
      assert packet_index(packet, 11) == "0"
      assert packet_index(packet, 12) == "123"
      assert packet_index(packet, 13) == "-1"
      assert packet_index(packet, 14) == "-1:-1:-1:10000.10000.1"
    end

    test "with non empty channel list" do
      mock = nstest_mock([channel_mock(), channel_mock()])
      packet = structure_to_iolist(mock)

      expected = "127.0.0.1:4000:0:1.1.ElvenGard 127.0.0.1:4000:0:1.1.ElvenGard"
      assert packet_index(packet, 13) == expected
    end
  end

  ## Helpers

  defp unused_server() do
    "-99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0 -99 0"
  end

  defp nstest_mock(server_list) do
    %NsTeSTPacket{
      encryption_key: 123,
      username: "admin",
      server_list: server_list
    }
  end

  defp channel_mock() do
    %Channel{
      id: 1,
      world_id: 1,
      world_name: "ElvenGard",
      ip: "127.0.0.1",
      port: 4000,
      population: 0
    }
  end
end
