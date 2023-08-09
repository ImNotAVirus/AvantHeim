defmodule ElvenViews.EntityViewsTest do
  use ViewCase, async: true

  alias ElvenViews.EntityViews

  alias ElvenViews.EntityPackets.{
    CharScPacket,
    CondPacket,
    EffPacket,
    StPacket,
    DirPacket,
    CModePacket
  }

  ## Tests

  describe "render:c_mode" do
    test "returns a packet structure" do
      mock = mock_c_mode()
      render = EntityViews.render(:c_mode, mock)

      assert %CModePacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.character.id
      assert is_integer(render.morph)
      assert is_integer(render.morph_upgrade)
      assert is_integer(render.morph_design)
      assert is_boolean(render.is_arena_winner)
      assert is_integer(render.size)
      assert is_integer(render.item_morph)
    end
  end

  describe "render:char_sc" do
    test "returns a packet structure" do
      mock = mock_char_sc()
      render = EntityViews.render(:char_sc, mock)

      assert %CharScPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert is_integer(render.size)
    end
  end

  describe "render:cond" do
    test "returns a packet structure" do
      mock = mock_cond()
      render = EntityViews.render(:cond, mock)

      assert %CondPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert is_boolean(render.no_attack)
      assert is_boolean(render.no_move)
      assert render.speed == mock.entity.speed
    end
  end

  describe "render:dir" do
    test "returns a packet structure" do
      mock = mock_dir()
      render = EntityViews.render(:dir, mock)

      assert %DirPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert render.direction == mock.entity.direction
    end
  end

  describe "render:eff" do
    test "returns a packet structure" do
      mock = mock_eff()
      render = EntityViews.render(:eff, mock)

      assert %EffPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert render.value == mock.value
    end
  end

  describe "render:st" do
    test "returns a packet structure" do
      mock = mock_st()
      render = EntityViews.render(:st, mock)

      assert %StPacket{} = render
      assert render.entity_type == :character
      assert render.entity_id == mock.entity.id
      assert render.level == mock.entity.level
      assert render.hero_level == mock.entity.hero_level
      assert is_integer(render.hp)
      assert is_integer(render.hp_percent)
      assert is_integer(render.mp)
      assert is_integer(render.mp_percent)
      assert render.buffs == mock.buffs
    end
  end

  ## Helpers

  defp mock_c_mode() do
    %{character: character_mock()}
  end

  defp mock_char_sc() do
    %{entity: character_mock()}
  end

  defp mock_cond() do
    %{entity: character_mock()}
  end

  defp mock_dir() do
    %{entity: character_mock()}
  end

  defp mock_eff() do
    %{entity: character_mock(), value: 123}
  end

  defp mock_st() do
    %{entity: character_mock(), buffs: []}
  end
end
