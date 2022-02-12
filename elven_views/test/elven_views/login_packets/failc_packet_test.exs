defmodule ElvenViews.LoginPackets.FailcPacketTest do
  use ExUnit.Case, async: true

  require ElvenViews.LoginPackets.FailcEnums

  alias ElvenViews.LoginPackets.FailcPacket
  alias ElvenViews.LoginPackets.FailcEnums

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      Enum.each(FailcEnums.error(:__enumerators__), fn {error, value} ->
        assert ["failc", ^value] = serialize_failc(error)
      end)
    end

    test "fallback to a generic error" do
      generic_error = FailcEnums.error(:generic)
      assert ["failc", ^generic_error] = serialize_failc(nil)
    end

    test "raises when error is invalid" do
      assert_raise ArgumentError, ~r/invalid value :foo/, fn ->
        serialize_failc(:foo)
      end
    end
  end

  ## Helpers

  defp serialize_failc(error) do
    %FailcPacket{error: error}
    |> FailcPacket.serialize([])
  end
end
