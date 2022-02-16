defmodule ElvenViews.SerializableEnumTest do
  use ExUnit.Case, async: true

  import ElvenCore.Socket.Serializer, only: [serialize_term: 2]

  alias ElvenViews.SerializableEnum

  ## Tests

  describe "new/1" do
    test "returns a structure" do
      assert %SerializableEnum{} = SerializableEnum.new(foo: 123)
    end

    test "arg must be a list" do
      assert_raise FunctionClauseError, fn ->
        SerializableEnum.new(:foo)
      end
    end
  end

  describe "SerializableEnum" do
    test "must implement the Serializable protocol" do
      enum = SerializableEnum.new(foo: 123, bar: 456)

      assert 123 = serialize_term(enum, for: :foo)
      assert 456 = serialize_term(enum, for: :bar)
    end

    test "raises an error if the key is not found" do
      assert_raise RuntimeError, ~r/key not found/, fn ->
        enum = SerializableEnum.new(foo: 123, bar: 456)
        serialize_term(enum, for: :test)
      end
    end
  end
end
