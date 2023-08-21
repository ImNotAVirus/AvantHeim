defmodule ElvenPackets.Types.NsEnumTest do
  use ExUnit.Case, async: true

  alias ElvenPackets.Types.NsEnum

  test "can be decoded" do
    opts = [values: [foo: 1, bar: 2]]

    assert {:foo, ""} = NsEnum.decode("1", opts)
    assert {:bar, ""} = NsEnum.decode("2", opts)
    assert {:foo, ""} = NsEnum.decode("1 ", opts)
    assert {:foo, "garbage"} = NsEnum.decode("1 garbage", opts)
    assert {:foo, "garbage with space"} = NsEnum.decode("1 garbage with space", opts)
  end

  test "can be encoded" do
    opts = [values: [foo: 1, bar: 2]]

    assert "1" = NsEnum.encode(:foo, opts)
    assert "2" = NsEnum.encode(:bar, opts)
  end
end
