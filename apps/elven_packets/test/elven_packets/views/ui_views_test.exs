defmodule ElvenPackets.Views.UIViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.UIViews
  alias ElvenPackets.SubPackets.I18nSubPacket
  alias ElvenPackets.Server.UiPackets.{Cancel, Gb, Gold, Info, Scene, SMemoi, SMemoi2}

  ## Tests

  describe "cancel" do
    test "default serialization for players" do
      args = %{entity: new_player(), cancel_type: :action}
      packet = UIViews.render(:cancel, args)

      assert %Cancel{} = packet
      assert packet.entity_id == args.entity.id
      assert packet.cancel_type == args.cancel_type
    end
  end

  describe "gb" do
    test "default serialization for players" do
      args = %{entity: new_player(), action_type: :open_from_savings_book}
      packet = UIViews.render(:gb, args)

      assert %Gb{} = packet
      assert packet.action_type == args.action_type
      assert packet.gold == 666
      assert packet.bank_gold == 123_456_789
      assert packet.bank_rank == 10
      assert packet.bank_tax == 20
    end
  end

  describe "gold" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = UIViews.render(:gold, args)

      assert %Gold{} = packet
      assert packet.gold == 666
      assert packet.bank_gold == 123_456_789
    end
  end

  describe "info" do
    test "default serialization" do
      args = %{message: "This is a message"}
      packet = UIViews.render(:info, args)

      assert %Info{} = packet
      assert packet.message == args.message
    end
  end

  describe "s_memoi" do
    test "default serialization" do
      args = %{i18n_key: "ThankYouForUsingTheCuarryBank"}
      packet = UIViews.render(:s_memoi, args)

      assert %SMemoi{} = packet
      assert packet.text_color == nil
      assert packet.i18n_packet == %I18nSubPacket{key: "ThankYouForUsingTheCuarryBank", args: []}
    end
  end

  describe "s_memoi2" do
    test "default serialization for players" do
      args = %{entity: new_player(), i18n_key: "BalanceBank"}
      packet = UIViews.render(:s_memoi2, args)

      assert %SMemoi2{} = packet
      assert packet.text_color == nil
      assert %I18nSubPacket{} = i18n = packet.i18n_packet
      assert i18n.key == args.i18n_key
      assert length(i18n.args) == 2
      # str(Bank gold / 1000)
      assert Enum.at(i18n.args, 0) == "123,456"
      # str(Gold)
      assert Enum.at(i18n.args, 1) == "666"
    end
  end

  describe "scene" do
    test "default serialization" do
      args = %{scene_id: 12}
      packet = UIViews.render(:scene, args)

      assert %Scene{} = packet
      assert packet.scene_id == args.scene_id
      assert packet.cancellable == true
    end
  end
end
