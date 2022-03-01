defmodule ElvenViews.ChatViewsTest do
  use ExUnit.Case, async: true

  alias ElvenCaching.Entity.Character
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

  def character_mock(attrs \\ %{}) do
    attrs |> character_attrs_mock() |> Character.new()
  end

  def character_attrs_mock(attrs \\ %{}) do
    Map.merge(
      %{
        id: 123,
        account_id: 456,
        name: "admin",
        gender: :male,
        class: :adventurer,
        hair_color: :dark_purple,
        hair_style: :hair_style_b,
        faction: :demon,
        map_vnum: 2,
        map_x: 3,
        map_y: 4,
        level: 5,
        job_level: 6,
        hero_level: 7,
        level_xp: 8,
        job_level_xp: 9,
        hero_level_xp: 10,
        gold: 11,
        bank_gold: 12,
        socket: :this_is_a_socket
      },
      attrs
    )
  end
end
