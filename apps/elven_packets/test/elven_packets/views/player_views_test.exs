defmodule ElvenPackets.Views.PlayerViewsTest do
  use ElvenPackets.ViewCase, async: true

  alias ElvenPackets.Views.PlayerViews
  alias ElvenPackets.SubPackets.Player.Family
  alias ElvenPackets.Server.PlayerPackets.{Fd, Fs, Lev, Rsfi, Stat, Tit, CInfo}

  ## Tests

  describe "c_info" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = PlayerViews.render(:c_info, args)

      assert %CInfo{} = packet
      assert packet.character_id == args.entity.id
      assert packet.name == "PlayerName"
      assert packet.group_id == -1
      assert packet.family == %Family{id: -1, rank: nil, name: nil}
      assert packet.authority == :player
      assert packet.gender == :male
      assert packet.hair_style == :hair_style_a
      assert packet.hair_color == :dark_purple
      assert packet.class == :archer
      assert packet.reputation_icon == :blue_nos
      assert packet.compliment == 500
      assert packet.morph == :default
      assert packet.is_invisible == false
      assert packet.family_level == nil
      assert packet.morph_upgrade == 0
      assert packet.wings_design == :default
      assert packet.is_arena_winner == false
    end
  end

  describe "fd" do
    test "default serialization for players" do
      entity = new_player()

      args = %{
        reputation: entity.reputation.reputation,
        reputation_icon: entity.reputation.reputation_icon,
        dignity: entity.reputation.dignity,
        dignity_icon: entity.reputation.dignity_icon
      }

      packet = PlayerViews.render(:fd, args)

      assert %Fd{} = packet
      assert packet.reputation == 11_1111
      assert packet.reputation_icon == :blue_nos
      assert packet.dignity == 100
      assert packet.dignity_icon == :basic
    end
  end

  describe "fs" do
    test "default serialization for players" do
      entity = new_player()
      args = %{faction: entity.faction.value}
      packet = PlayerViews.render(:fs, args)

      assert %Fs{} = packet
      assert packet.faction == :demon
    end
  end

  describe "lev" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = PlayerViews.render(:lev, args)

      assert %Lev{} = packet
      assert packet.level == 99
      assert packet.level_xp == 999_999
      assert packet.level_xp_max == 9_000_000
      assert packet.job_level == 89
      assert packet.job_level_xp == 888_888
      assert packet.job_level_xp_max == 8_000_000
      assert packet.hero_level == 79
      assert packet.hero_level_xp == 777_777
      assert packet.hero_level_xp_max == 7_000_000
      assert packet.reputation == 111_111
      assert packet.cp == 0
    end
  end

  describe "rsfi" do
    test "default serialization for players" do
      args = %{}
      packet = PlayerViews.render(:rsfi, args)

      assert %Rsfi{} = packet
      assert packet.act == 1
      assert packet.act_part == 1
      assert packet.ts == 0
      assert packet.ts_max == 0
    end
  end

  describe "stat" do
    test "default serialization for players" do
      args = %{entity: new_player()}
      packet = PlayerViews.render(:stat, args)

      assert %Stat{} = packet
      assert packet.hp == 40_000
      assert packet.hp_max == 44_444
      assert packet.mp == 30_000
      assert packet.mp_max == 33_333
      assert packet.option == 0
    end
  end

  describe "tit" do
    test "default serialization for players" do
      entity = new_player()

      args = %{
        class: entity.player.class,
        name: entity.player.name
      }

      packet = PlayerViews.render(:tit, args)

      assert %Tit{} = packet
      assert packet.class == :archer
      assert packet.name == "PlayerName"
    end
  end
end
