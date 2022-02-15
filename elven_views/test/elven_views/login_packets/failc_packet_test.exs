defmodule ElvenViews.LoginPackets.FailcPacketTest do
  use PacketCase, async: true

  require ElvenViews.LoginPackets.FailcEnums

  alias ElvenViews.LoginPackets.FailcPacket
  alias ElvenViews.LoginPackets.FailcEnums

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      mock = failc_mock()
      packet = serialize(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "failc"
      assert packet_index(packet, 1) == FailcEnums.error(mock.error, :value)
    end

    test "fallback to a generic error" do
      packet = nil |> failc_mock() |> serialize()
      assert packet_index(packet, 1) == FailcEnums.error(:generic)
    end

    test "raises when error is invalid" do
      assert_raise ArgumentError, ~r/invalid value :foo/, fn ->
        :foo |> failc_mock() |> serialize()
      end
    end
  end

  ## Helpers

  defp failc_mock(error \\ :bad_credentials) do
    %FailcPacket{error: error}
  end
end
