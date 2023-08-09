defmodule ElvenPackets.Types.NsIntegerTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.NsInteger

  test "can be deserialized" do
    assert {1337, ""} = NsInteger.decode("1337")
    assert {1337, ""} = NsInteger.decode("1337 ")
    assert {1337, "garbage"} = NsInteger.decode("1337 garbage")
    assert {1337, "garbage"} = NsInteger.decode("1337\vgarbage")
    assert {1337, "garbage with space"} = NsInteger.decode("1337 garbage with space")
  end

  test "can be serialized" do
    assert "1337" = NsInteger.encode(1337)
  end
end
