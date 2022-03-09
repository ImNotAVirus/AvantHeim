defmodule ElvenViews.PlayerViewsTest do
  use ViewCase, async: true

  alias ElvenViews.PlayerViews
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket

  alias ElvenViews.PlayerPackets.{
    CInfoPacket,
    CModePacket,
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
      assert is_struct(render.family_id_rank, FamilyIdRankSubPacket)
      assert is_binary(render.family_name) or render.family_name == nil
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

  ## Helpers

  defp mock_c_info() do
    %{character: character_mock()}
  end
end
