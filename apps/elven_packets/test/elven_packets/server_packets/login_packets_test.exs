defmodule ElvenPackets.Server.LoginPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.LoginPackets.{Failc, NsTeST}

  ## Tests

  test "can serialize failc" do
    assert {"failc", ["1"]} = serialize_packet(%Failc{error: :old_client})
    assert {"failc", ["2"]} = serialize_packet(%Failc{error: :generic})
    assert {"failc", ["4"]} = serialize_packet(%Failc{error: :already_connected})
    assert {"failc", ["5"]} = serialize_packet(%Failc{error: :bad_credentials})

    # Test default value
    assert {"failc", ["2"]} = serialize_packet(%Failc{})
  end

  test "can serialize NsTeST" do
    server_list = []

    packet = %NsTeST{
      region: :fr,
      username: "admin",
      encryption_key: 123,
      server_list: []
    }

    {header, params} = serialize_packet(packet)

    assert :foo = [header | params] |> Enum.intersperse(" ") |> :erlang.list_to_binary()
  end
end
