defmodule ElvenViews.LoginPackets.NsTeSTPacketTest do
  use ExUnit.Case, async: true

  alias ElvenViews.LoginPackets.NsTeSTPacket
  alias ElvenViews.LoginPackets.NsTeST.Channel

  @encryption_key 123
  @username "admin"

  ## Tests

  describe "serialize/2" do
    test "can serialize non empty channel list" do
      channel = mock_channel()

      assert [
               "NsTeST",
               region,
               username,
               auth_type,
               _server1,
               _server2,
               _server3,
               _server4,
               _server5,
               _server6,
               _unused,
               0,
               encryption_key,
               serialized_servers,
               terminator
             ] = serialize_NsTeST([channel])

      assert is_integer(region)
      assert username == @username
      assert is_integer(auth_type)
      assert encryption_key == @encryption_key
      assert serialized_servers == Channel.serialize(channel, [])
      assert terminator == "-1:-1:-1:10000.10000.1"
    end

    test "can serialize empty channel list" do
      packet = serialize_NsTeST([])
      assert packet_index(packet, 13) == "-1"
    end
  end

  ## Helpers

  defp packet_index(packet, index), do: Enum.at(packet, index)

  defp serialize_NsTeST(server_list) do
    %NsTeSTPacket{
      encryption_key: @encryption_key,
      username: @username,
      server_list: server_list
    }
    |> NsTeSTPacket.serialize([])
  end

  defp mock_channel() do
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
