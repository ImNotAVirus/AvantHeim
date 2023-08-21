defmodule ElvenViews.ChatViewsTest do
  use ViewCase, async: true

  alias ElvenViews.ChatViews
  alias ElvenViews.ChatPackets.{BnPacket, SayPacket}

  ## Tests

  describe "render:bn" do
    test "returns a packet structure" do
      mock = mock_bn()
      render = ChatViews.render(:bn, mock)

      assert %BnPacket{} = render
      assert render.id == mock.id
      assert render.message == mock.message
    end
  end

  describe "render:say" do
    test "returns a packet structure" do
      mock = mock_say()
      render = ChatViews.render(:say, mock)

      assert %SayPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert render.message == mock.message
      assert render.color == mock.color
    end
  end

  ## Helpers

  defp mock_bn() do
    %{id: 123, message: "This is a message"}
  end

  defp mock_say() do
    %{entity: character_mock(), message: "This is a message", color: :special_gold}
  end
end
