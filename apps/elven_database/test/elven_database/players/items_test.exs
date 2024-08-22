defmodule ElvenDatabase.Players.ItemsTest do
  use ElvenDatabase.RepoCase, async: true

  alias ElvenDatabase.Players.{Accounts, Characters, Item, Items}

  ## Setup

  setup ctx do
    accounts_count = Map.get(ctx, :accounts, 1)
    characters_count = Map.get(ctx, :characters, 1)

    # Create accounts for each test
    accounts =
      for _ <- 1..accounts_count do
        Accounts.create!(account_attrs())
      end

    # Create characters for each account
    characters =
      for account <- accounts, _ <- 1..characters_count do
        Characters.create!(character_attrs(account.id))
      end

    # Return state
    %{accounts: accounts, characters: characters}
  end

  ## Tests

  describe "create/1" do
    test "can create an item with owner", %{characters: [character]} do
      attrs = %{
        owner: character,
        inventory_type: :etc,
        slot: 10,
        vnum: 2,
        quantity: 3
      }

      assert {:ok, item} = Items.create(attrs)
      assert %Item{} = item
      assert item.owner_id == character.id
      assert item.inventory_type == :etc
      assert item.slot == 10
      assert item.vnum == 2
      assert item.quantity == 3
    end

    test "can create an item with owner_id", %{characters: [character]} do
      attrs = %{
        owner_id: character.id,
        inventory_type: :etc,
        slot: 10,
        vnum: 2,
        quantity: 3
      }

      assert {:ok, item} = Items.create(attrs)
      assert %Item{} = item
      assert item.owner_id == character.id
      assert item.inventory_type == :etc
      assert item.slot == 10
      assert item.vnum == 2
      assert item.quantity == 3
    end

    test "can create an item with a slot as atom", %{characters: [character]} do
      attrs = %{
        owner_id: character.id,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      assert {:ok, item} = Items.create(attrs)
      assert item.slot == 5
    end

    test "owner must exists" do
      attrs = %{
        owner_id: 30_000,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      assert {:error, changeset} = Items.create(attrs)
      assert changeset_error(changeset) == "owner_id does not exist"
    end

    @tag characters: 2
    test "slot must be unique by owner + inventory_type + slot", %{characters: characters} do
      [character1, character2] = characters

      attrs = %{
        owner_id: character1.id,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      # First insert is fine
      assert {:ok, _item} = Items.create(attrs)

      # Using same inventory_type + slot but another owner_id is fine
      assert {:ok, _item} = Items.create(%{attrs | owner_id: character2.id})

      # Using same owner_id + slot but another inventory_type is fine
      assert {:ok, _item} = Items.create(%{attrs | inventory_type: :etc})

      # Using same owner_id + inventory_type but another slot is fine
      assert {:ok, _item} = Items.create(%{attrs | slot: :main_weapon})

      # Same owner + inventory_type + slot return an error
      assert {:error, changeset} = Items.create(attrs)
      assert changeset_error(changeset) == "slot has already been taken"
    end
  end

  describe "create!/1" do
    test "can create an item using owner", %{characters: [character]} do
      attrs = %{
        owner: character,
        inventory_type: :etc,
        slot: 10,
        vnum: 2,
        quantity: 3
      }

      assert %Item{} = item = Items.create!(attrs)
      assert item.owner_id == character.id
      assert item.inventory_type == :etc
      assert item.slot == 10
      assert item.vnum == 2
      assert item.quantity == 3
    end

    test "can create an item using owner_id", %{characters: [character]} do
      attrs = %{
        owner_id: character.id,
        inventory_type: :etc,
        slot: 10,
        vnum: 2,
        quantity: 3
      }

      assert %Item{} = item = Items.create!(attrs)
      assert item.owner_id == character.id
      assert item.inventory_type == :etc
      assert item.slot == 10
      assert item.vnum == 2
      assert item.quantity == 3
    end

    test "can create an item with a slot as atom", %{characters: [character]} do
      attrs = %{
        owner_id: character.id,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      item = Items.create!(attrs)
      assert item.slot == 5
    end

    test "owner must exists" do
      attrs = %{
        owner_id: 30_000,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      assert_raise Ecto.InvalidChangesetError, fn ->
        Items.create!(attrs)
      end
    end

    @tag characters: 2
    test "slot must be unique by owner + inventory_type + slot", %{characters: characters} do
      [character1, character2] = characters

      attrs = %{
        owner_id: character1.id,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      # First insert is fine
      _item = Items.create!(attrs)

      # Using same inventory_type + slot but another owner_id is fine
      _item = Items.create!(%{attrs | owner_id: character2.id})

      # Using same owner_id + slot but another inventory_type is fine
      _item = Items.create!(%{attrs | inventory_type: :etc})

      # Using same owner_id + inventory_type but another slot is fine
      _item = Items.create!(%{attrs | slot: :main_weapon})

      # Same owner + inventory_type + slot raise an error
      assert_raise Ecto.InvalidChangesetError, fn ->
        Items.create!(attrs)
      end
    end
  end

  describe "get/1" do
    test "get item by id", %{characters: [character]} do
      item1 =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert Items.get(item1.id) == {:ok, item1}
      assert Items.get(10_000) == {:error, :not_found}
    end
  end

  describe "get!/1" do
    test "get item by id", %{characters: [character]} do
      item1 =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert Items.get!(item1.id) == item1

      assert_raise Ecto.NoResultsError, fn ->
        Items.get!(10_000)
      end
    end
  end

  describe "list_by_owner/1" do
    @tag characters: 2
    test "list items by owner", %{characters: characters} do
      [character1, character2] = characters

      item1 =
        Items.create!(%{
          owner: character1,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      item2 =
        Items.create!(%{
          owner: character1,
          inventory_type: :etc,
          slot: 1,
          vnum: 2,
          quantity: 3
        })

      assert Items.list_by_owner(character1.id) == [item1, item2]
      assert Items.list_by_owner(character2.id) == []
    end
  end

  describe "update/2" do
    test "update an item", %{characters: [character]} do
      item =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert Items.update(item, %{slot: 42}) == {:ok, %Item{item | slot: 42}}
    end
  end

  describe "update!/2" do
    test "update an item", %{characters: [character]} do
      item =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert Items.update!(item, %{slot: 42}) == %Item{item | slot: 42}
    end
  end

  describe "delete/1" do
    test "delete an item", %{characters: [character]} do
      item =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert {:ok, %Item{}} = Items.delete(item)
      assert Items.get(item.id) == {:error, :not_found}

      # Can't delete an item 2 times
      assert_raise Ecto.StaleEntryError, fn ->
        Items.delete(item)
      end
    end
  end

  describe "delete!/1" do
    test "delete an item", %{characters: [character]} do
      item =
        Items.create!(%{
          owner: character,
          inventory_type: :etc,
          slot: 10,
          vnum: 2,
          quantity: 3
        })

      assert %Item{} = Items.delete!(item)
      assert Items.get(item.id) == {:error, :not_found}

      # Can't delete an item 2 times
      assert_raise Ecto.StaleEntryError, fn ->
        Items.delete!(item)
      end
    end
  end
end
