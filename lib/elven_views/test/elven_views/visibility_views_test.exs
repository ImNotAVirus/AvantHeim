defmodule ElvenViews.VisibilityViewsTest do
  use ViewCase, async: true

  alias ElvenViews.VisibilityViews

  alias ElvenViews.SubPackets.EquipmentSubPacket
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket
  alias ElvenViews.SubPackets.ItemUpgradeRaritySubPacket

  alias ElvenViews.VisibilityPackets.{InCharacterPacket, OutPacket}

  ## Tests

  describe "render:in" do
    test "returns a packet structure" do
      mock = mock_in()
      render = VisibilityViews.render(:in, mock)

      assert %InCharacterPacket{} = render
      assert render.name == mock.entity.name
      assert render.entity_id == mock.entity.id
      assert render.map_x == mock.entity.map_x
      assert render.map_y == mock.entity.map_y
      assert render.direction == mock.entity.direction
      assert is_atom(render.authority)
      assert render.gender == mock.entity.gender
      assert render.hair_style == mock.entity.hair_style
      assert render.hair_color == mock.entity.hair_color
      assert render.class == mock.entity.class
      assert %EquipmentSubPacket{} = render.equipments
      assert is_integer(render.hp_percent)
      assert is_integer(render.mp_percent)
      assert render.is_sitting == mock.entity.is_sitting
      assert is_integer(render.group_id)
      assert is_integer(render.fairy_move_type_id)
      assert is_atom(render.fairy_element)
      assert is_integer(render.fairy_morph)
      assert render.spawn_effect == mock.spawn_effect
      assert is_integer(render.morph)
      assert %ItemUpgradeRaritySubPacket{} = render.weapon_upgrade
      assert %ItemUpgradeRaritySubPacket{} = render.armor_upgrade
      assert %FamilyIdRankSubPacket{} = render.family_id_rank
      assert is_binary(render.family_name) or is_nil(render.family_name)
      assert is_integer(render.reputation_icon_id)
      assert is_boolean(render.is_invisible)
      assert is_integer(render.morph_upgrade)
      assert render.faction == mock.entity.faction
      assert is_integer(render.morph_design)
      assert render.level == mock.entity.level
      assert is_integer(render.family_level)
      assert is_list(render.family_icons)
      assert is_boolean(render.is_arena_winner)
      assert is_integer(render.compliment)
      assert is_integer(render.size)
      assert is_integer(render.hero_level)
      assert is_integer(render.title_id)
    end
  end

  describe "render:out" do
    test "returns a packet structure" do
      mock = mock_out()
      render = VisibilityViews.render(:out, mock)

      assert %OutPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
    end
  end

  ## Helpers

  defp mock_in() do
    %{
      entity: character_mock(),
      equipments: [nil, nil, nil, nil, nil, nil, nil, nil, nil, nil],
      spawn_effect: :falling
    }
  end

  defp mock_out() do
    %{entity: character_mock()}
  end
end
