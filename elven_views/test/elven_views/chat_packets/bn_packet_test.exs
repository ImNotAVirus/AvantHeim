defmodule ElvenViews.ChatPackets.BnPacketTest do
  use ExUnit.Case, async: true

  alias ElvenViews.ChatPackets.BnPacket

  @id 123
  @message "Hello welcome to AvantHeim"

  ## Tests

  describe "serialize/2" do
    test "can serialize a packet structure" do
      assert ["bn", id, message] = serialize_bn()
      assert id == @id
      assert message == escape(@message)
    end
  end

  ## Helpers

  defp serialize_bn() do
    %BnPacket{id: @id, message: @message}
    |> BnPacket.serialize([])
  end

  defp escape(message) do
    String.replace(message, " ", "^")
  end
end
