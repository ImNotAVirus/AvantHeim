defmodule ElvenPackets.Types.NsStringTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.NsString

  test "can be deserialized" do
    assert {"data", ""} = NsString.decode("data")
    assert {"data", ""} = NsString.decode("data ")
    assert {"data", "garbage"} = NsString.decode("data garbage")
    assert {"data", "garbage"} = NsString.decode("data\vgarbage")
    assert {"data", "garbage with space"} = NsString.decode("data garbage with space")
  end

  test "can be serialized" do
    assert "data" = NsString.encode("data")
  end
end
