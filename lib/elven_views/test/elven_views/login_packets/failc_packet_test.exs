defmodule ElvenViews.LoginPackets.FailcPacketTest do
  use PacketCase, async: true

  alias ElvenViews.LoginPackets.FailcPacket

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      mock = failc_mock(:bad_credentials)
      packet = structure_to_iolist(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "failc"
      assert packet_index(packet, 1) == "5"
    end

    test "fallback to a generic error" do
      mock = failc_mock(nil)
      packet = structure_to_iolist(mock)

      assert packet_index(packet, 1) == "2"
    end

    test "raises when error is invalid" do
      assert_raise ArgumentError, ~r/invalid key :foo in enum/, fn ->
        :foo |> failc_mock() |> structure_to_iolist()
      end
    end
  end

  ## Helpers

  defp failc_mock(error) do
    %FailcPacket{error: error}
  end
end
