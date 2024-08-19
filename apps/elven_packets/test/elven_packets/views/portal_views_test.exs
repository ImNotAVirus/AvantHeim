defmodule ElvenPackets.Views.PortalViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.PortalViews
  alias ElvenPackets.Server.PortalPackets.Gp

  ## Tests

  describe "gp" do
    test "default serialization" do
      args = %{
        source_x: 123,
        source_y: 456,
        map_id: 7,
        portal_type: -1
      }

      packet = PortalViews.render(:gp, args)

      assert %Gp{} = packet
      assert packet.source_x == args.source_x
      assert packet.source_y == args.source_y
      assert packet.map_id == args.map_id
      assert packet.portal_type == args.portal_type
      assert packet.portal_direction == :north
      assert packet.is_disabled == false
    end

    test "serialization with portal_direction" do
      args = %{
        source_x: 123,
        source_y: 456,
        map_id: 7,
        portal_type: -1,
        portal_direction: :east
      }

      packet = PortalViews.render(:gp, args)
      assert packet.portal_direction == :east
    end

    test "serialization with is_disabled" do
      args = %{
        source_x: 123,
        source_y: 456,
        map_id: 7,
        portal_type: -1,
        is_disabled: true
      }

      packet = PortalViews.render(:gp, args)
      assert packet.is_disabled == true
    end
  end
end
