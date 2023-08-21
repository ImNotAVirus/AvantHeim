defmodule ElvenI18n.PacketConstStringTest do
  use ExUnit.Case, async: true

  alias ElvenI18n.PacketConstString

  ## Tests

  describe "new/1,2" do
    test "returns i18n info without args" do
      expected = %{key: "TestSubject", value: 2342, args: []}
      assert {:ok, ^expected} = PacketConstString.new("TestSubject")
    end

    test "returns i18n info with args" do
      expected = %{key: "DepositBank", value: 2343, args: ["123"]}
      assert {:ok, ^expected} = PacketConstString.new("DepositBank", ["123"])

      expected = %{key: "WithdrawBank", value: 2344, args: ["123", "456"]}
      assert {:ok, ^expected} = PacketConstString.new("WithdrawBank", ["123", "456"])
    end

    test "returns an error tuple when args length doesn't match" do
      assert {:error, :badargs} = PacketConstString.new("TestSubject", ["123"])
      assert {:error, :badargs} = PacketConstString.new("WithdrawBank", ["123"])
    end

    test "returns an error tuple when args types doesn't match" do
      assert {:error, :badargs} = PacketConstString.new("DepositBank", [123])
      assert {:error, :badargs} = PacketConstString.new("WithdrawBank", [123, "456"])
    end
  end

  describe "new!/1,2" do
    test "returns i18n info without args" do
      expected = %{key: "TestSubject", value: 2342, args: []}
      assert ^expected = PacketConstString.new!("TestSubject")
    end

    test "returns i18n info with args" do
      expected = %{key: "DepositBank", value: 2343, args: ["123"]}
      assert ^expected = PacketConstString.new!("DepositBank", ["123"])

      expected = %{key: "WithdrawBank", value: 2344, args: ["123", "456"]}
      assert ^expected = PacketConstString.new!("WithdrawBank", ["123", "456"])
    end

    test "raises an error when args length doesn't match" do
      assert_raise RuntimeError, ~r/badargs/, fn ->
        PacketConstString.new!("TestSubject", ["123"])
      end

      assert_raise RuntimeError, ~r/badargs/, fn ->
        PacketConstString.new!("WithdrawBank", ["123"])
      end
    end

    test "raises an error when args types doesn't match" do
      assert_raise RuntimeError, ~r/badargs/, fn ->
        PacketConstString.new!("DepositBank", [123])
      end

      assert_raise RuntimeError, ~r/badargs/, fn ->
        PacketConstString.new!("WithdrawBank", [123, "456"])
      end
    end
  end
end
