defmodule ElvenPackets.Types.NsBooleanTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.NsBoolean

  test "can be decoded" do
    assert {true, ""} = NsBoolean.decode("1")
    assert {false, ""} = NsBoolean.decode("0")
    assert {true, "garbage"} = NsBoolean.decode("1 garbage")
    assert {true, "garbage"} = NsBoolean.decode("1\vgarbage")
    assert {true, "garbage with space"} = NsBoolean.decode("1 garbage with space")
  end

  test "can be encoded" do
    assert "1" = NsBoolean.encode(true)
    assert "0" = NsBoolean.encode(false)
  end
end
