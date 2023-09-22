defmodule ElvenPackets.Views.EntityViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.EntityViews
  alias ElvenPackets.Server.EntityPackets.{CMode, CharSc, Cond, Dir, Eff, St}

  ## Tests

  describe "c_mode" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = EntityViews.render(:c_mode, args)

      assert %CMode{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.morph == :default
      assert packet.morph_upgrade == 0
      assert packet.wings_design == :default
      assert packet.is_arena_winner == false
      assert packet.size == 20
      assert packet.item_morph == 0
    end
  end

  describe "char_sc" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = EntityViews.render(:char_sc, args)

      assert %CharSc{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.size == 20
    end
  end

  describe "cond" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = EntityViews.render(:cond, args)

      assert %Cond{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.no_attack == false
      assert packet.no_move == false
      assert packet.speed == 40
    end
  end

  describe "dir" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = EntityViews.render(:dir, args)

      assert %Dir{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.direction == :south
    end
  end

  describe "eff" do
    test "default serialization for players" do
      args = %{entity: new_player(), value: 1337}
      packet = EntityViews.render(:eff, args)

      assert %Eff{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.value == 1337
    end
  end

  describe "st" do
    test "default serialization for players" do
      args = %{entity: new_player(), buffs: []}
      packet = EntityViews.render(:st, args)

      assert %St{} = packet
      assert packet.entity_type == :player
      assert packet.entity_id == args.entity.id
      assert packet.level == 99
      assert packet.hero_level == 79
      assert packet.hp == 40000
      assert packet.hp_percent == 90
      assert packet.mp == 30000
      assert packet.mp_percent == 90
      assert packet.buffs == args.buffs
    end
  end
end
