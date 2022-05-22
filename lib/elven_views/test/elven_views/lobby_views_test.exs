defmodule ElvenViews.LobbyViewsTest do
  use ViewCase, async: true

  alias ElvenViews.LobbyViews

  alias ElvenViews.SubPackets.EquipmentSubPacket

  alias ElvenViews.LobbyPackets.{
    ClistEndPacket,
    ClistPacket,
    ClistStartPacket,
    OkPacket
  }

  ## Tests

  describe "render:clist_start" do
    test "returns a packet structure" do
      mock = mock_clist_start()
      render = LobbyViews.render(:clist_start, mock)

      assert %ClistStartPacket{} = render
    end
  end

  describe "render:clist_end" do
    test "returns a packet structure" do
      mock = mock_clist_end()
      render = LobbyViews.render(:clist_end, mock)

      assert %ClistEndPacket{} = render
    end
  end

  describe "render:ok" do
    test "returns a packet structure" do
      mock = mock_ok()
      render = LobbyViews.render(:ok, mock)

      assert %OkPacket{} = render
    end
  end

  describe "render:clist" do
    test "returns a packet structure" do
      mock = mock_clist()
      render = LobbyViews.render(:clist, mock)

      assert %ClistPacket{} = render
      assert render.slot == mock.character.slot
      assert render.name == mock.character.name
      assert render.gender == mock.character.gender
      assert render.hair_style == mock.character.hair_style
      assert render.hair_color == mock.character.hair_color
      assert render.class == mock.character.class
      assert render.level == mock.character.level
      assert render.hero_level == mock.character.hero_level
      assert %EquipmentSubPacket{} = render.equipments
      assert render.job_level == mock.character.job_level
      assert render.pets == mock.pets
      assert render.design == mock.design
    end
  end

  ## Helpers

  defp mock_clist_start() do
    %{}
  end

  defp mock_clist_end() do
    %{}
  end

  defp mock_ok() do
    %{}
  end

  defp mock_clist() do
    %{
      character: Map.put(character_mock(), :slot, 11),
      equipments: [nil, nil, nil, nil, nil, nil, nil, nil, nil, nil],
      pets: [],
      design: 0
    }
  end
end
