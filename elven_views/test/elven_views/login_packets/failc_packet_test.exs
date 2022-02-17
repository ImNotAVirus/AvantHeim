defmodule ElvenViews.LoginPackets.FailcPacketTest do
  use PacketCase, async: true

  require ElvenViews.LoginPackets.FailcEnums

  alias ElvenViews.LoginPackets.FailcPacket
  alias ElvenViews.LoginPackets.FailcEnums

  ## Tests

  describe "serialize/2" do
    test "can serialize the structure" do
      mock = failc_mock()
      packet = serialize_structure(mock)

      assert is_list(packet)
      assert packet_index(packet, 0) == "failc"
      assert packet_index(packet, 1) == FailcEnums.error(mock.error, :value)
    end

    test "fallback to a generic error" do
      mock = failc_mock(nil)
      packet = serialize_structure(mock)

      assert packet_index(packet, 1) == structure_enum_default(mock, :error)
    end

    test "raises when error is invalid" do
      assert_raise ArgumentError, ~r/invalid key :foo in enum/, fn ->
        :foo |> failc_mock() |> serialize_structure()
      end
    end
  end

  ## Helpers

  defp failc_mock(error \\ :bad_credentials) do
    %FailcPacket{error: error}
  end
end
