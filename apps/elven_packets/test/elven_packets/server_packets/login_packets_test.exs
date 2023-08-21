defmodule ElvenPackets.Server.LoginPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.LoginPackets.{Failc, NsTeST}
  alias ElvenPackets.SubPackets.Login.NsTeST.Channel

  ## Tests

  describe "failc" do
    test "can be serialized" do
      assert {"failc", ["1"]} = serialize_packet(%Failc{error: :old_client})
      assert {"failc", ["2"]} = serialize_packet(%Failc{error: :generic})
      assert {"failc", ["4"]} = serialize_packet(%Failc{error: :already_connected})
      assert {"failc", ["5"]} = serialize_packet(%Failc{error: :bad_credentials})

      # Test default value
      assert {"failc", ["2"]} = serialize_packet(%Failc{})
    end
  end

  describe "NsTeST" do
    test "can be serialized without servers" do
      assert {"NsTeST", params} = serialize_packet(nstest())
      assert is_list(params)
      assert length(params) == 14
      assert Enum.at(params, 0) == "2"
      assert Enum.at(params, 1) == "admin"
      assert Enum.at(params, 2) == "2"
      assert Enum.at(params, 3) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 4) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 5) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 6) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 7) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 8) == "-99 0 -99 0 -99 0 -99 0"
      assert Enum.at(params, 9) == unused_server()
      assert Enum.at(params, 10) == "0"
      assert Enum.at(params, 11) == "123"
      assert Enum.at(params, 12) == "-1"
      assert Enum.at(params, 13) == "-1:-1:-1:10000.10000.1"
    end

    test "can be serialized with servers" do
      packet = nstest(%{server_list: [channel(1, 1)]})
      assert {"NsTeST", params} = serialize_packet(packet)
      assert Enum.at(params, 12) == ["127.0.0.1:4001:0:1.1.ElvenGard"]

      packet = nstest(%{server_list: [channel(1, 1), channel(2, 2)]})
      assert {"NsTeST", params} = serialize_packet(packet)

      assert Enum.at(params, 12) == [
               "127.0.0.1:4001:0:1.1.ElvenGard",
               "127.0.0.1:4002:0:2.2.ElvenGard"
             ]
    end
  end

  ## Helpers

  defp unused_server(), do: List.duplicate("-99 0 -99 0 -99 0 -99 0", 3)

  defp nstest(attrs \\ %{}) do
    %NsTeST{
      region: :fr,
      username: "admin",
      encryption_key: 123,
      server_list: []
    }
    |> Map.merge(attrs)
  end

  defp channel(world_id, channel_id) do
    %Channel{
      id: world_id,
      world_id: channel_id,
      world_name: "ElvenGard",
      ip: "127.0.0.1",
      port: 4000 + channel_id,
      population: 0
    }
  end
end
