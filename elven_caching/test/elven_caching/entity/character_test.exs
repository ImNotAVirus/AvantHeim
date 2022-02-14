defmodule ElvenCaching.Entity.CharacterTest do
  use EntityCase, async: true

  alias ElvenCaching.Entity
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
    test "get_position/1" do
      character = Character.new(character_attrs_mock())
      assert %EntityPosition{} = Entity.get_position(character)
    end

    test "set_position/2" do
      character = Character.new(character_attrs_mock())
      position = EntityPosition.new(123, 456, 789)

      assert %Character{map_id: 123, map_vnum: 123, map_x: 456, map_y: 789} =
               Entity.set_position(character, position)
    end
  end
end
