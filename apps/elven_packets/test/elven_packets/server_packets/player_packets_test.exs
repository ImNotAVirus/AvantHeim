defmodule ElvenPackets.Client.PlayerPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.SubPackets.Player.CInfo.Family
  alias ElvenPackets.Server.PlayerPackets.{Fd, Fs, Lev, Rsfi, Stat, Tit, CInfo}

  ## Tests

  describe "c_info" do
    test "can be serialized" do
      packet = c_info()
      assert {"c_info", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 18
      assert Enum.at(params, 0) == "Fizo"
      assert Enum.at(params, 1) == "-"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "1.918 >>Trying"
      assert Enum.at(params, 4) == "1"
      assert Enum.at(params, 5) == "0"
      assert Enum.at(params, 6) == "0"
      assert Enum.at(params, 7) == "1"
      assert Enum.at(params, 8) == "0"
      assert Enum.at(params, 9) == "2"
      assert Enum.at(params, 10) == "23"
      assert Enum.at(params, 11) == "456"
      assert Enum.at(params, 12) == "50"
      assert Enum.at(params, 13) == "0"
      assert Enum.at(params, 14) == "2"
      assert Enum.at(params, 15) == "3"
      assert Enum.at(params, 16) == "0"
      assert Enum.at(params, 17) == "1"
    end
  end

  describe "fd" do
    test "can be serialized" do
      packet = %Fd{
        reputation: 1,
        reputation_icon: :blue_nos,
        dignity: 3,
        dignity_icon: :stupid_minded
      }

      assert {"fd", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 4
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "23"
      assert Enum.at(params, 2) == "3"
      assert Enum.at(params, 3) == "6"
    end
  end

  describe "fs" do
    test "can be serialized" do
      packet = %Fs{faction: :angel}
      assert {"fs", [params]} = serialize_packet(packet)
      assert params == "1"
    end
  end

  describe "lev" do
    test "can be serialized" do
      packet = lev()
      assert {"lev", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 12
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "2"
      assert Enum.at(params, 2) == "3"
      assert Enum.at(params, 3) == "4"
      assert Enum.at(params, 4) == "5"
      assert Enum.at(params, 5) == "6"
      assert Enum.at(params, 6) == "7"
      assert Enum.at(params, 7) == "8"
      assert Enum.at(params, 8) == "9"
      assert Enum.at(params, 9) == "10"
      assert Enum.at(params, 10) == "11"
      assert Enum.at(params, 11) == "12"
    end
  end

  describe "rsfi" do
    test "can be serialized" do
      packet = %Rsfi{act: 1, act_part: 2, ts: 5, ts_max: 6}
      assert {"rsfi", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 6
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "2"
      assert Enum.at(params, 2) == "3"
      assert Enum.at(params, 3) == "4"
      assert Enum.at(params, 4) == "5"
      assert Enum.at(params, 5) == "6"
    end
  end

  describe "stat" do
    test "can be serialized" do
      packet = %Stat{hp: 1, hp_max: 2, mp: 3, mp_max: 4, option: 6}
      assert {"stat", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 6
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "2"
      assert Enum.at(params, 2) == "3"
      assert Enum.at(params, 3) == "4"
      assert Enum.at(params, 4) == "5"
      assert Enum.at(params, 5) == "6"
    end
  end

  describe "tit" do
    test "can be serialized" do
      packet = %Tit{class: :archer, name: "Fizo"}
      assert {"tit", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 2
      assert Enum.at(params, 0) == "37"
      assert Enum.at(params, 1) == "Fizo"
    end
  end

  ## Helpers

  defp c_info() do
    %CInfo{
      name: "Fizo",
      group_id: 1,
      family: %Family{id: 1, rank: :member, name: ">>Trying"},
      character_id: 1,
      authority: :player,
      gender: :male,
      hair_style: :hair_style_b,
      hair_color: :dark_purple,
      class: :archer,
      reputation_icon: :blue_nos,
      compliment: 456,
      morph: :pet_trainer_skin,
      is_invisible: false,
      family_level: 2,
      morph_upgrade: 3,
      is_arena_winner: true
    }
  end

  defp lev() do
    %Lev{
      level: 1,
      level_xp: 2,
      job_level: 3,
      job_level_xp: 4,
      level_xp_max: 5,
      job_level_xp_max: 6,
      reputation: 7,
      cp: 8,
      hero_level_xp: 9,
      hero_level: 10,
      hero_level_xp_max: 11,
      unknown: 12
    }
  end
end
