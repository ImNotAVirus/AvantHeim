defmodule ElvenPackets.Types.NsListTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.{NsInteger, NsList}

  test "can be decoded" do
    assert {["foo"], ""} = NsList.decode("foo", joiner: " ")
    assert {["foo", "bar"], ""} = NsList.decode("foo bar", joiner: " ")
    assert {["foo"], "bar"} = NsList.decode("foo bar", joiner: "|")
    assert {["foo|bar", "baz"], ""} = NsList.decode("foo|bar baz", joiner: " ")
    assert {["foo", "bar"], "baz"} = NsList.decode("foo|bar baz", joiner: "|")
    assert {[1, 3, 3, 7], "foo"} = NsList.decode("1|3|3|7 foo", joiner: "|", type: NsInteger)
  end

  test "can be encoded" do
    assert ["foo", "bar"] = NsList.encode(["foo", "bar"], joiner: " ")
    assert "foo|bar" = NsList.encode(["foo", "bar"], joiner: "|")
    assert ["1", "2"] = NsList.encode([1, 2], joiner: " ", type: NsInteger)
    assert "1|2" = NsList.encode([1, 2], joiner: "|", type: NsInteger)
  end
end
