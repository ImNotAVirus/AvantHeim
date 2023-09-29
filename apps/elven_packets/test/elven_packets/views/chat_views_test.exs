defmodule ElvenPackets.Views.ChatViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.ChatViews
  alias ElvenPackets.Server.ChatPackets.{Bn, Say}

  ## Tests

  describe "bn" do
    test "default serialization" do
      args = %{id: 123, message: "This is a message"}
      packet = ChatViews.render(:bn, args)

      assert %Bn{} = packet
      assert packet.id == args.id
      assert packet.message == args.message
    end
  end

  describe "say" do
    test "default serialization for players" do
      args = %{entity_type: :player, entity_id: 123, message: "This is a message"}
      packet = ChatViews.render(:say, args)

      assert %Say{} = packet
      assert packet.entity_type == args.entity_type
      assert packet.entity_id == args.entity_id
      assert packet.message == args.message
      assert packet.color == nil
    end

    test "serialization for players with color" do
      args = %{
        entity_type: :player,
        entity_id: 123,
        message: "This is a message",
        color: :special_gold
      }

      packet = ChatViews.render(:say, args)

      assert %Say{} = packet
      assert packet.entity_type == args.entity_type
      assert packet.entity_id == args.entity_id
      assert packet.message == args.message
      assert packet.color == args.color
    end
  end
end
