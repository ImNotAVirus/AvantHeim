defmodule ElvenPackets.Server.LoginPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.LoginPackets.{Failc}

  ## Tests

  test "can serialize Failc" do
    assert {"failc", ["1"]} = serialize_packet(%Failc{error: :old_client})
    assert {"failc", ["2"]} = serialize_packet(%Failc{error: :generic})
    assert {"failc", ["4"]} = serialize_packet(%Failc{error: :already_connected})
    assert {"failc", ["5"]} = serialize_packet(%Failc{error: :bad_credentials})

    # Test default value
    assert {"failc", ["2"]} = serialize_packet(%Failc{})
  end
end
