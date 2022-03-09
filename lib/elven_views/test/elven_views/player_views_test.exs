defmodule ElvenViews.PlayerViewsTest do
  use ExUnit.Case, async: true

  alias ElvenCaching.Entity.Character
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

  def character_mock(attrs \\ %{}) do
    attrs |> character_attrs_mock() |> Character.new()
  end

  def character_attrs_mock(attrs \\ %{}) do
    Map.merge(
      %{
        id: 123,
        account_id: 456,
        name: "admin",
        gender: :male,
        class: :adventurer,
        hair_color: :dark_purple,
        hair_style: :hair_style_b,
        faction: :demon,
        map_vnum: 2,
        map_x: 3,
        map_y: 4,
        level: 5,
        job_level: 6,
        hero_level: 7,
        level_xp: 8,
        job_level_xp: 9,
        hero_level_xp: 10,
        gold: 11,
        bank_gold: 12,
        socket: :this_is_a_socket
      },
      attrs
    )
  end
end
