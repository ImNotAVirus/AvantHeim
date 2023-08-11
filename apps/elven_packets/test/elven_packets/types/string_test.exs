defmodule ElvenPackets.Types.NsStringTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.NsString

  test "can be decoded" do
    assert {"data", ""} = NsString.decode("data")
    assert {"data", ""} = NsString.decode("data ")
    assert {"data", "garbage"} = NsString.decode("data garbage")
    assert {"data", "garbage"} = NsString.decode("data\vgarbage")
    assert {"data", "garbage with space"} = NsString.decode("data garbage with space")
  end

  test "can be encoded" do
    assert "data" = NsString.encode("data")
  end

  test "support escape option for encoding" do
    assert "data^with^space" = NsString.encode("data with space", escape: true)
  end

  test "support full option for decoding" do
    assert {"data with space", ""} = NsString.decode("data with space", full: true)
  end
end
