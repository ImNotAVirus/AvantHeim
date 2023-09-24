defmodule ElvenPackets.Views.VisibilityViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.VisibilityViews
  alias ElvenPackets.Server.VisibilityPackets.{InPlayer, Out}
  alias ElvenPackets.SubPackets.Equipment
  alias ElvenPackets.SubPackets.Item.UpgradeRarity
  alias ElvenPackets.SubPackets.Player.Family

  ## Tests

  describe "in" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = VisibilityViews.render(:in, args)

      assert %InPlayer{} = packet
      assert packet.name == "PlayerName"
      assert packet.entity_id == args.entity.id
      assert packet.map_x == 12
      assert packet.map_y == 34
      assert packet.direction == :south
      assert packet.authority == :player
      assert packet.gender == :male
      assert packet.hair_style == :hair_style_a
      assert packet.hair_color == :dark_purple
      assert packet.class == :archer

      assert packet.equipments == %Equipment{
               hat: nil,
               armor: nil,
               main_weapon: nil,
               secondary_weapon: nil,
               mask: nil,
               fairy: nil,
               costume_suit: nil,
               costume_hat: nil,
               weapon_skin: nil,
               wings_skin: nil
             }

      assert packet.hp_percent == 90
      assert packet.mp_percent == 90
      assert packet.is_sitting == false
      assert packet.group_id == -1
      assert packet.fairy_move_type_id == -1
      assert packet.fairy_element == :neutral
      assert packet.fairy_morph == -1
      assert packet.spawn_effect == :summon
      assert packet.morph == :default
      assert packet.weapon_upgrade == %UpgradeRarity{upgrade: 0, rarity: 0}
      assert packet.armor_upgrade == %UpgradeRarity{upgrade: 0, rarity: 0}
      assert packet.family == %Family{id: -1, rank: nil, name: nil}
      assert packet.reputation_icon == :blue_nos
      assert packet.is_invisible == false
      assert packet.morph_upgrade == 0
      assert packet.faction == :demon
      assert packet.wings_design == :default
      assert packet.level == 99
      assert packet.family_level == nil
      assert packet.family_icons == []
      assert packet.is_arena_winner == false
      assert packet.compliment == 500
      assert packet.size == 20
      assert packet.hero_level == 79
      assert packet.title_id == 10
    end
  end

  describe "out" do
    test "default serialization for players" do
      args = %{entity_type: :player, entity_id: 123}
      packet = VisibilityViews.render(:out, args)

      assert %Out{} = packet
      assert packet.entity_type == args.entity_type
      assert packet.entity_id == args.entity_id
    end
  end
end
