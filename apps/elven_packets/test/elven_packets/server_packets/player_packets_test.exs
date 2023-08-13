defmodule ElvenPackets.Client.PlayerPacketsTest do
  use ElvenPackets.PacketCase, async: true

  alias ElvenPackets.SubPackets.Player.CInfo.FamilyIdRank
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
      assert Enum.at(params, 2) == 1
      assert Enum.at(params, 3) == "1.918"
      assert Enum.at(params, 4) == 1
      assert Enum.at(params, 5) == 1
      assert Enum.at(params, 6) == 2
      assert Enum.at(params, 7) == 1
      assert Enum.at(params, 8) == 2
      assert Enum.at(params, 9) == 2
      assert Enum.at(params, 10) == 123
      assert Enum.at(params, 11) == 456
      assert Enum.at(params, 12) == 789
      assert Enum.at(params, 13) == false
      assert Enum.at(params, 14) == 2
      assert Enum.at(params, 15) == 3
      assert Enum.at(params, 16) == 4
      assert Enum.at(params, 17) == true
    end
  end

  describe "fd" do
    test "can be serialized" do
      packet = fd()
      assert {"fd", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 4
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "1"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "1"
    end
  end

  describe "fs" do
    test "can be serialized" do
      packet = fs()
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
      assert Enum.at(params, 1) == "1"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "1"
      assert Enum.at(params, 4) == "1"
      assert Enum.at(params, 5) == "1"
      assert Enum.at(params, 6) == "1"
      assert Enum.at(params, 7) == "1"
      assert Enum.at(params, 8) == "1"
      assert Enum.at(params, 9) == "1"
      assert Enum.at(params, 10) == "1"
      assert Enum.at(params, 11) == "1"
    end
  end

  describe "rsfi" do
    test "can be serialized" do
      packet = rsfi()
      assert {"rsfi", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 6
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "1"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "1"
      assert Enum.at(params, 4) == "1"
      assert Enum.at(params, 5) == "1"
    end
  end

  describe "stat" do
    test "can be serialized" do
      packet = stat()
      assert {"stat", params} = serialize_packet(packet)
      assert is_list(params)
      assert length(params) == 6
      assert Enum.at(params, 0) == "1"
      assert Enum.at(params, 1) == "1"
      assert Enum.at(params, 2) == "1"
      assert Enum.at(params, 3) == "1"
      assert Enum.at(params, 4) == "1"
      assert Enum.at(params, 5) == "1"
    end
  end

  describe "tit" do
    test "can be serialized" do
      packet = tit()
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
      family_id_rank: [%FamilyIdRank{id: 1, rank: :member}],
      character_id: 1,
      authority: :player,
      gender: :male,
      hair_style: :hair_style_b,
      hair_color: :darke_purple,
      class: :archer,
      reputation_icon_id: 123,
      family_name: "test",
      compliment: 456,
      morph: 789,
      is_invisible: false,
      family_level: 2,
      morph_upgrade: 3,
      morph_design: 4,
      is_arena_winner: true
    }
  end

  defp fd() do
    %Fd{
      reputation: 1,
      reputation_icon_id: 1,
      dignity: 1,
      dignity_icon_id: 1
    }
  end

  defp fs() do
    %Fs{faction: :angel}
  end

  defp stat() do
    %Stat{
      hp: 1,
      hp_max: 1,
      mp: 1,
      mp_max: 1,
      unknown: 1,
      option: 1
    }
  end

  defp rsfi() do
    %Rsfi{
      act: 1,
      act_part: 1,
      unknown: 1,
      unknown2: 1,
      ts: 1,
      ts_max: 1
    }
  end

  defp lev() do
    %Lev{
      level: 1,
      level_xp: 1,
      job_level: 1,
      job_level_xp: 1,
      level_xp_max: 1,
      job_level_xp_max: 1,
      reputation: 1,
      cp: 1,
      hero_level_xp: 1,
      hero_level: 1,
      hero_level_xp_max: 1,
      unknown: 1
    }
  end

  defp tit() do
    %Tit{
      class: :archer,
      name: "Fizo"
    }
  end
end
