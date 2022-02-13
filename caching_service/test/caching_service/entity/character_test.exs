defmodule CachingService.Entity.CharacterTest do
  use ExUnit.Case, async: true

  alias CachingService.Entity
  alias CachingService.Entity.Character
  alias CachingService.Entity.EntityPosition

  ## Tests

  describe "new/1" do
    test "create a new structure" do
      assert %Character{} = Character.new(character_mock())
    end

    test "raises if a attribute is missing" do
      assert_raise ArgumentError, "missing attributes: [:socket]", fn ->
        character_mock()
        |> Map.delete(:socket)
        |> Character.new()
      end

      assert_raise ArgumentError, "missing attributes: [:gender, :socket]", fn ->
        character_mock()
        |> Map.delete(:socket)
        |> Map.delete(:gender)
        |> Character.new()
      end
    end
  end

  describe "Entity protocol" do
    test "get_position/1" do
      character = Character.new(character_mock())
      assert %EntityPosition{} = Entity.get_position(character)
    end

    test "set_position/2" do
      character = Character.new(character_mock())
      position = EntityPosition.new(123, 456, 789)

      assert %Character{map_id: 123, map_vnum: 123, map_x: 456, map_y: 789} =
               Entity.set_position(character, position)
    end
  end

  ## Helpers

  defp character_mock() do
    %{
      id: 1,
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
    }
  end
end
