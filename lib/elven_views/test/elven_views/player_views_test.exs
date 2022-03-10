defmodule ElvenViews.PlayerViewsTest do
  use ViewCase, async: true

  alias ElvenViews.PlayerViews
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket

  alias ElvenViews.PlayerPackets.{
    CInfoPacket,
    FdPacket,
    FsPacket,
    LevPacket,
    RsfiPacket,
    StatPacket,
    TitPacket
  }

  ## Tests

  describe "render:c_info" do
    test "returns a packet structure" do
      mock = mock_c_info()
      render = PlayerViews.render(:c_info, mock)

      assert %CInfoPacket{} = render
      assert render.character_id == mock.character.id
      assert render.name == mock.character.name
      assert is_integer(render.group_id)
      assert %FamilyIdRankSubPacket{} = render.family_id_rank
      assert is_binary(render.family_name) or is_nil(render.family_name)
      assert is_atom(render.authority)
      assert render.gender == mock.character.gender
      assert render.hair_style == mock.character.hair_style
      assert render.hair_color == mock.character.hair_color
      assert render.class == mock.character.class
      assert is_integer(render.reputation_icon_id)
      assert is_integer(render.compliment)
      assert is_integer(render.morph)
      assert is_boolean(render.is_invisible)
      assert is_integer(render.family_level)
      assert is_integer(render.morph_upgrade)
      assert is_integer(render.morph_design)
      assert is_boolean(render.is_arena_winner)
    end
  end

  describe "render:fd" do
    test "returns a packet structure" do
      mock = mock_fd()
      render = PlayerViews.render(:fd, mock)

      assert %FdPacket{} = render
      assert is_integer(render.reputation)
      assert is_integer(render.reputation_icon_id)
      assert is_integer(render.dignity)
      assert is_integer(render.dignity_icon_id)
    end
  end

  describe "render:fs" do
    test "returns a packet structure" do
      mock = mock_fs()
      render = PlayerViews.render(:fs, mock)

      assert %FsPacket{} = render
      assert is_atom(render.faction)
    end
  end

  describe "render:lev" do
    test "returns a packet structure" do
      mock = mock_lev()
      render = PlayerViews.render(:lev, mock)

      assert %LevPacket{} = render
      assert render.level == mock.character.level
      assert render.level_xp == mock.character.level_xp
      assert is_integer(render.level_xp_max)
      assert render.job_level == mock.character.job_level
      assert render.job_level_xp == mock.character.job_level_xp
      assert is_integer(render.job_level_xp_max)
      assert render.hero_level == mock.character.hero_level
      assert render.hero_level_xp == mock.character.hero_level_xp
      assert is_integer(render.hero_level_xp_max)
      assert is_integer(render.reputation)
      assert is_integer(render.cp)
    end
  end

  describe "render:rsfi" do
    test "returns a packet structure" do
      mock = mock_rsfi()
      render = PlayerViews.render(:rsfi, mock)

      assert %RsfiPacket{} = render
      assert render.act == mock.act
      assert render.act_part == mock.act_part
      assert render.ts == mock.ts
      assert render.ts_max == mock.ts_max
    end
  end

  describe "render:stat" do
    test "returns a packet structure" do
      mock = mock_stat()
      render = PlayerViews.render(:stat, mock)

      assert %StatPacket{} = render
      assert is_integer(render.hp)
      assert is_integer(render.hp_max)
      assert is_integer(render.mp)
      assert is_integer(render.mp_max)
      assert render.option == mock.option
    end
  end

  describe "render:tit" do
    test "returns a packet structure" do
      mock = mock_tit()
      render = PlayerViews.render(:tit, mock)

      assert %TitPacket{} = render
      assert render.class == mock.character.class
      assert render.name == mock.character.name
    end
  end

  ## Helpers

  defp mock_c_info() do
    %{character: character_mock()}
  end

  defp mock_fd() do
    %{character: character_mock()}
  end

  defp mock_fs() do
    %{character: character_mock()}
  end

  defp mock_lev() do
    %{character: character_mock()}
  end

  defp mock_rsfi() do
    %{act: 1, act_part: 2, ts: 3, ts_max: 4}
  end

  defp mock_stat() do
    %{character: character_mock(), option: 1}
  end

  defp mock_tit() do
    %{character: character_mock()}
  end
end
