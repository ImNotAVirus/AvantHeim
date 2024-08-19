defmodule ElvenPackets.Server.PortalPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.Server.PortalPackets.Gp

  ## Tests
  describe "gp" do
    test "can be serialized" do
      packet = %Gp{
        source_x: 123,
        source_y: 456,
        map_id: 7,
        portal_type: -1,
        portal_direction: :south,
        is_disabled: false
      }

      assert {"gp", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 6
      assert Enum.at(params, 0) == "123"
      assert Enum.at(params, 1) == "456"
      assert Enum.at(params, 2) == "7"
      assert Enum.at(params, 3) == "-1"
      assert Enum.at(params, 4) == "2"
      assert Enum.at(params, 5) == "0"
    end
  end
end
