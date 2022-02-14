defmodule ElvenCaching.CharacterRegistryTest do
  use EntityCase, async: true

  alias ElvenCaching.CharacterRegistry
  alias ElvenCaching.Entity.Character

  ## Setup

  setup_all do
    registry = start_supervised!(CharacterRegistry)
    RegistryTestHelpers.sync(registry)
    :ok
  end

  setup do
    {:ok, id: random_integer()}
  end

  ## Tests

  describe "create/1" do
    test "creates a new Character", %{id: id} do
      assert [] = get_character(id)
      assert {:ok, %Character{}} = create_character(id)
      assert [_] = get_character(id)
    end
  end

  describe "write/1" do
    test "creates a new Character if not exists", %{id: id} do
      character = character_mock(%{id: id})

      assert [] = get_character(id)
      assert {:ok, ^character} = CharacterRegistry.write(character)
      assert [_] = get_character(id)
    end

    test "updates an existing Character", %{id: id} do
      {:ok, character} = create_character(id)
      updated_character = %Character{character | name: "ThisIsANewName"}

      assert [_] = get_character(id)
      assert {:ok, ^updated_character} = CharacterRegistry.write(updated_character)
      assert [cached_character] = get_character(id)
      assert attribute_index(cached_character, 2) == "ThisIsANewName"
    end
  end

  describe "get/1" do
    test "returns an existing character", %{id: id} do
      {:ok, character} = create_character(id)
      assert {:ok, ^character} = CharacterRegistry.get(id)
    end

    test "returns :not_found if not existing", %{id: id} do
      assert {:error, :not_found} = CharacterRegistry.get(id)
    end
  end

  describe "get_by_name/1" do
    test "returns an existing character", %{id: _id} do
      {:ok, character} = create_character()
      assert {:ok, ^character} = CharacterRegistry.get_by_name(character.name)
    end

    test "returns :not_found if not existing", %{id: _id} do
      assert {:error, :not_found} = CharacterRegistry.get_by_name("NonExisting")
    end
  end

  describe "delete_by_account_id/1" do
    test "deletes an existing character", %{id: id} do
      {:ok, character} = create_character_with_acc_id(id)

      assert [_] = get_character(character.id)
      assert {:ok, ^character} = CharacterRegistry.delete_by_account_id(id)
      assert [] = get_character(character.id)
    end

    test "returns :not_found if not existing", %{id: id} do
      assert {:error, :not_found} = CharacterRegistry.delete_by_account_id(id)
    end
  end

  describe "get_by_map_id/1,2" do
    test "returns an empty list when no character", %{id: id} do
      assert {:ok, []} = CharacterRegistry.get_by_map_id(id)
    end

    test "returns existing characters", %{id: id} do
      {:ok, character} = create_character_with_map_id(id)
      assert {:ok, [^character]} = CharacterRegistry.get_by_map_id(id)

      {:ok, character2} = create_character_with_map_id(id)
      assert {:ok, [_, _] = characters} = CharacterRegistry.get_by_map_id(id)
      assert character in characters
      assert character2 in characters
    end

    test "returns existing characters with guards", %{id: id} do
      {:ok, character} = create_character_with_map_id(id)
      {:ok, character2} = create_character_with_map_id(id)

      assert {:ok, [^character]} = CharacterRegistry.get_by_map_id(id, {:==, :id, character.id})
      assert {:ok, [^character2]} = CharacterRegistry.get_by_map_id(id, {:!=, :id, character.id})
      assert {:ok, [^character2]} = CharacterRegistry.get_by_map_id(id, {:==, :id, character2.id})
      assert {:ok, [^character]} = CharacterRegistry.get_by_map_id(id, {:!=, :id, character2.id})
    end
  end

  ## Helpers

  defp create_character() do
    character_attrs_mock() |> CharacterRegistry.create()
  end

  defp create_character(id) do
    %{id: id} |> character_attrs_mock() |> CharacterRegistry.create()
  end

  defp create_character_with_acc_id(id) do
    %{account_id: id} |> character_attrs_mock() |> CharacterRegistry.create()
  end

  defp create_character_with_map_id(id) do
    %{map_vnum: id} |> character_attrs_mock() |> CharacterRegistry.create()
  end

  defp get_character(id) do
    :mnesia.dirty_read({Character, id})
  end

  defp attribute_index(tuple, index) do
    # +1 because the first elem is the record name
    elem(tuple, index + 1)
  end
end
