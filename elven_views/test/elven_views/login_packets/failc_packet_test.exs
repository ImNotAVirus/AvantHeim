defmodule ElvenViews.LoginPackets.FailcPacketTest do
  use ExUnit.Case, async: true

  alias ElvenViews.LoginPackets.FailcPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      assert ["failc", 1] = serialize_failc(:old_client)
      assert ["failc", 4] = serialize_failc(:already_connected)
      assert ["failc", 5] = serialize_failc(:bad_credentials)
    end

    test "fallback to a generic error" do
      assert ["failc", 2] = serialize_failc(nil)
    end
  end

  ## Helpers

  defp serialize_failc(error) do
    %FailcPacket{error: error}
    |> FailcPacket.serialize([])
  end
end
