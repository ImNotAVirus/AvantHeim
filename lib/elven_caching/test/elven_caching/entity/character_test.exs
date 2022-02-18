defmodule ElvenCaching.Entity.CharacterTest do
  use EntityCase, async: true

  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.LevelableEntity
  alias ElvenCaching.BattleEntity

  alias ElvenCaching.Entity.Character

  ## Tests

  describe "new/1" do
    test "create a new structure" do
      assert %Character{} = Character.new(character_attrs_mock())
    end

    test "raises if a attribute is missing" do
      assert_raise ArgumentError, "missing attributes: [:socket]", fn ->
        character_attrs_mock()
        |> Map.delete(:socket)
        |> Character.new()
      end

      assert_raise ArgumentError, "missing attributes: [:gender, :socket]", fn ->
        character_attrs_mock()
        |> Map.delete(:socket)
        |> Map.delete(:gender)
        |> Character.new()
      end
    end
  end

  describe "Character implements protocol" do
    test "ElvenCaching.Entity" do
      character = Character.new(character_attrs_mock())
      assert Entity.impl_for(character)
    end

    test "ElvenCaching.MapEntity" do
      character = Character.new(character_attrs_mock())
      assert MapEntity.impl_for(character)
    end

    test "ElvenCaching.LevelableEntity" do
      character = Character.new(character_attrs_mock())
      assert LevelableEntity.impl_for(character)
    end

    test "ElvenCaching.BattleEntity" do
      character = Character.new(character_attrs_mock())
      assert BattleEntity.impl_for(character)
    end
  end
end
