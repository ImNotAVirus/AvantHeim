defmodule ElvenCaching.Entity.CharacterTest do
  use EntityCase, async: true

  alias ElvenCaching.Entity
  alias ElvenCaching.MovableEntity
  alias ElvenCaching.Entity.Character
  alias ElvenCaching.Entity.EntityPosition

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

  describe "Entity protocol" do
    test "type/1" do
      character = Character.new(character_attrs_mock())
      assert Entity.type(character) == :character
    end

    test "id/2" do
      character = Character.new(character_attrs_mock())
      assert Entity.id(character) == character.id
    end
  end

  describe "MovableEntity protocol" do
    test "position/1" do
      character = Character.new(character_attrs_mock())
      assert %EntityPosition{} = MovableEntity.position(character)
    end

    test "position/2" do
      character = Character.new(character_attrs_mock())
      position = EntityPosition.new(123, 456, 789)

      assert %Character{map_id: 123, map_vnum: 123, map_x: 456, map_y: 789} =
               MovableEntity.position(character, position)
    end
  end
end
