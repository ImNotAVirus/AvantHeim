defmodule ElvenViews.UIViewsTest do
  use ViewCase, async: true

  alias ElvenViews.UIViews

  alias ElvenViews.SubPackets.I18nSubPacket

  alias ElvenViews.UIPackets.{
    CancelPacket,
    GbPacket,
    GoldPacket,
    InfoPacket,
    SMemoiPacket,
    SMemoi2Packet,
    ScenePacket
  }

  ## Tests

  describe "render:cancel" do
    test "returns a packet structure" do
      mock = mock_cancel()
      render = UIViews.render(:cancel, mock)

      assert %CancelPacket{} = render
      assert render.cancel_type == mock.cancel_type
      assert render.entity_id == mock.entity.id
    end
  end

  describe "render:gb" do
    test "returns a packet structure" do
      mock = mock_gb()
      render = UIViews.render(:gb, mock)

      assert %GbPacket{} = render
      assert render.action_type == mock.action_type
      assert render.bank_gold == mock.character.bank_gold
      assert render.gold == mock.character.gold
      assert render.bank_rank == mock.bank_rank
      assert render.bank_tax == mock.bank_tax
    end
  end

  describe "render:gold" do
    test "returns a packet structure" do
      mock = mock_gold()
      render = UIViews.render(:gold, mock)

      assert %GoldPacket{} = render
      assert render.bank_gold == mock.character.bank_gold
      assert render.gold == mock.character.gold
    end
  end

  describe "render:info" do
    test "returns a packet structure" do
      mock = mock_info()
      render = UIViews.render(:info, mock)

      assert %InfoPacket{} = render
      assert render.message == mock.message
    end
  end

  describe "render:s_memoi" do
    test "returns a packet structure" do
      mock = mock_s_memoi()
      render = UIViews.render(:s_memoi, mock)

      assert %SMemoiPacket{} = render
      assert render.text_color == mock.text_color
      assert %I18nSubPacket{} = render.i18n_packet
    end
  end

  describe "render:s_memoi2" do
    test "returns a packet structure" do
      mock = mock_s_memoi2()
      render = UIViews.render(:s_memoi2, mock)

      assert %SMemoi2Packet{} = render
      assert render.text_color == mock.text_color
      assert %I18nSubPacket{args: args} = render.i18n_packet
      assert length(args) == 2
    end
  end

  describe "render:scene" do
    test "returns a packet structure" do
      mock = mock_scene()
      render = UIViews.render(:scene, mock)

      assert %ScenePacket{} = render
      assert render.scene_id == mock.scene_id
      assert render.cancellable == mock.cancellable
    end
  end

  ## Helpers

  defp mock_cancel() do
    %{cancel_type: :skill, entity: character_mock()}
  end

  defp mock_gb() do
    %{
      character: character_mock(),
      action_type: :open_from_savings_book,
      bank_rank: 11,
      bank_tax: 22
    }
  end

  defp mock_gold() do
    %{character: character_mock()}
  end

  defp mock_info() do
    %{message: "This is a message"}
  end

  defp mock_s_memoi() do
    %{
      text_color: :green,
      i18n_key: "ThankYouForUsingTheCuarryBank"
    }
  end

  defp mock_s_memoi2() do
    %{
      text_color: :green,
      i18n_key: "BalanceBank",
      character: character_mock()
    }
  end

  defp mock_scene() do
    %{
      scene_id: 123,
      cancellable: true
    }
  end
end
