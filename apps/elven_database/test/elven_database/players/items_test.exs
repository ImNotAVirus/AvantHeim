defmodule ElvenDatabase.Players.ItemsTest do
  use ElvenDatabase.RepoCase, async: true

  alias ElvenDatabase.Players.{Accounts, Characters, Item, Items}

  ## Setup

  setup do
    # Create an account for each test
    account = Accounts.create!(account_attrs())

    # Each account have a character
    character = Characters.create!(character_attrs(account.id))

    # Return state
    %{account: account, character: character}
  end

  ## Tests

  describe "create/1" do
    test "can create an item", %{character: character} do
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

    test "can create an item with a slot as atom", %{character: character} do
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

    test "slot must be unique", %{character: character} do
      attrs = %{
        owner_id: character.id,
        inventory_type: :equipped,
        slot: :secondary_weapon,
        vnum: 1,
        quantity: 1
      }

      # First insert is fine
      assert {:ok, _item} = Items.create(attrs)

      # Same slot return an error
      assert {:error, changeset} = Items.create(attrs)
      assert changeset_error(changeset) == "slot has already been taken"
    end
  end
end
