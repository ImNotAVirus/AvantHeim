defmodule ElvenDatabase.Players.CharactersTest do
  use ElvenDatabase.RepoCase, async: true

  alias ElvenDatabase.Players.{Accounts, Character, Characters, Item}

  ## Setup

  setup ctx do
    accounts_count = Map.get(ctx, :accounts, 1)

    # Create accounts for each test
    accounts =
      for _ <- 1..accounts_count do
        Accounts.create!(account_attrs())
      end

    # Return state
    %{accounts: accounts}
  end

  ## Tests

  describe "create/1" do
    test "can create a character with account", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # Check structure returned by create/1
      assert {:ok, character} = Characters.create(attrs)
      assert %Character{} = character
      assert character.account_id == attrs.account.id
      assert character.name == attrs.name
      assert character.slot == attrs.slot
      assert character.gender == attrs.gender
      assert character.hair_color == attrs.hair_color
      assert character.hair_style == attrs.hair_style
      assert character.map_id == attrs.map_id
      assert character.map_x == attrs.map_x
      assert character.map_y == attrs.map_y

      # Check data inserted
      character = Characters.get!(character.id)

      assert character.account_id == attrs.account.id
      assert character.name == attrs.name
      assert character.slot == attrs.slot
      assert character.gender == attrs.gender
      assert character.hair_color == attrs.hair_color
      assert character.hair_style == attrs.hair_style
      assert character.map_id == attrs.map_id
      assert character.map_x == attrs.map_x
      assert character.map_y == attrs.map_y
      # Check default fields
      assert character.class == :adventurer
      assert character.faction == :neutral
      assert character.additional_hp == 0
      assert character.additional_mp == 0
      assert character.gold == 0
      assert character.bank_gold == 0
      assert character.biography == "Hi!"
      assert character.level == 1
      assert character.job_level == 1
      assert character.hero_level == 0
      assert character.level_xp == 0
      assert character.job_level_xp == 0
      assert character.hero_level_xp == 0
      assert character.sp_points == 10_000
      assert character.sp_additional_points == 50_000
      assert character.rage_points == 0
      assert character.max_mate_count == 10
      assert character.reputation == 0
      assert character.dignity == 100
      assert character.compliment == 0
      assert character.act4_dead == 0
      assert character.act4_kill == 0
      assert character.act4_points == 0
      assert character.arena_winner == false
      assert character.talent_win == 0
      assert character.talent_lose == 0
      assert character.talent_surrender == 0
      assert character.master_points == 0
      assert character.master_ticket == 0
      assert character.miniland_intro == "Welcome!"
      assert character.miniland_state == :open
      assert character.miniland_makepoints == 2000
    end

    test "can create a character with account_id", %{accounts: [account]} do
      attrs = %{
        account_id: account.id,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert {:ok, %Character{}} = Characters.create(attrs)
    end

    test "can create a character with custom fields", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113,
        # Non default fields
        class: :archer,
        faction: :angel,
        gold: 2_000_000,
        arena_winner: true
      }

      # Check structure returned by create/1
      assert {:ok, %Character{} = character} = Characters.create(attrs)
      assert character.class == attrs.class
      assert character.faction == attrs.faction
      assert character.gold == attrs.gold
      assert character.arena_winner == attrs.arena_winner

      # Check data inserted
      character = Characters.get!(character.id)

      assert character.class == attrs.class
      assert character.faction == attrs.faction
      assert character.gold == attrs.gold
      assert character.arena_winner == attrs.arena_winner
    end

    test "can create a character with items", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113,
        items: [
          item_attrs(%{slot: 0}),
          item_attrs(%{slot: 1}),
          item_attrs(%{slot: 2})
        ]
      }

      assert {:ok, character} = Characters.create(attrs)

      # We need to preload items first
      character = Characters.preload_items(character)

      assert length(character.items) == 3
    end

    test "must have a valid name", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: "test",
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert {:error, changeset} = Characters.create(%{attrs | name: "foo"})
      assert changeset_error(changeset) == "name should be at least 4 character(s)"

      assert {:error, changeset} = Characters.create(%{attrs | name: "foofoofoofoofoo"})
      assert changeset_error(changeset) == "name should be at most 14 character(s)"

      assert {:error, changeset} = Characters.create(%{attrs | name: " spacebefore"})
      assert changeset_error(changeset) == "name has invalid format"

      assert {:error, changeset} = Characters.create(%{attrs | name: "spaceafter "})
      assert changeset_error(changeset) == "name has invalid format"

      assert {:error, changeset} = Characters.create(%{attrs | name: "space between"})
      assert changeset_error(changeset) == "name has invalid format"

      assert {:error, changeset} = Characters.create(%{attrs | name: "おはよう"})
      assert changeset_error(changeset) == "name has invalid format"
    end

    test "name must be unique", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # First insert is fine
      assert {:ok, _character} = Characters.create(attrs)

      # Same name returns an error
      assert {:error, changeset} = Characters.create(attrs)
      assert changeset_error(changeset) == "name has already been taken"
    end

    @tag accounts: 2
    test "slot must be unique by account + slot", %{accounts: accounts} do
      [account1, account2] = accounts

      attrs = %{
        account: account1,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # First insert is fine
      assert {:ok, _character} = Characters.create(attrs)

      # Using same slot but another account is fine
      assert {:ok, _character} =
               Characters.create(%{attrs | account: account2, name: random_string()})

      # Using same account but another slot is fine
      assert {:ok, _character} = Characters.create(%{attrs | slot: 2, name: random_string()})

      # Same account + slot return an error
      assert {:error, changeset} = Characters.create(%{attrs | name: random_string()})
      assert changeset_error(changeset) == "slot has already been taken"
    end
  end

  describe "create!/1" do
    test "can create a character with account", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # Check structure returned by create!/1
      assert %Character{} = character = Characters.create!(attrs)
      assert character.account_id == attrs.account.id
      assert character.name == attrs.name
      assert character.slot == attrs.slot
      assert character.gender == attrs.gender
      assert character.hair_color == attrs.hair_color
      assert character.hair_style == attrs.hair_style
      assert character.map_id == attrs.map_id
      assert character.map_x == attrs.map_x
      assert character.map_y == attrs.map_y

      # Check data inserted
      character = Characters.get!(character.id)

      assert character.account_id == attrs.account.id
      assert character.name == attrs.name
      assert character.slot == attrs.slot
      assert character.gender == attrs.gender
      assert character.hair_color == attrs.hair_color
      assert character.hair_style == attrs.hair_style
      assert character.map_id == attrs.map_id
      assert character.map_x == attrs.map_x
      assert character.map_y == attrs.map_y
      # Check default fields
      assert character.class == :adventurer
      assert character.faction == :neutral
      assert character.additional_hp == 0
      assert character.additional_mp == 0
      assert character.gold == 0
      assert character.bank_gold == 0
      assert character.biography == "Hi!"
      assert character.level == 1
      assert character.job_level == 1
      assert character.hero_level == 0
      assert character.level_xp == 0
      assert character.job_level_xp == 0
      assert character.hero_level_xp == 0
      assert character.sp_points == 10_000
      assert character.sp_additional_points == 50_000
      assert character.rage_points == 0
      assert character.max_mate_count == 10
      assert character.reputation == 0
      assert character.dignity == 100
      assert character.compliment == 0
      assert character.act4_dead == 0
      assert character.act4_kill == 0
      assert character.act4_points == 0
      assert character.arena_winner == false
      assert character.talent_win == 0
      assert character.talent_lose == 0
      assert character.talent_surrender == 0
      assert character.master_points == 0
      assert character.master_ticket == 0
      assert character.miniland_intro == "Welcome!"
      assert character.miniland_state == :open
      assert character.miniland_makepoints == 2000
    end

    test "can create a character with account_id", %{accounts: [account]} do
      attrs = %{
        account_id: account.id,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert %Character{} = Characters.create!(attrs)
    end

    test "can create a character with custom fields", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113,
        # Non default fields
        class: :archer,
        faction: :angel,
        gold: 2_000_000,
        arena_winner: true
      }

      # Check structure returned by create/1
      assert %Character{} = character = Characters.create!(attrs)
      assert character.class == attrs.class
      assert character.faction == attrs.faction
      assert character.gold == attrs.gold
      assert character.arena_winner == attrs.arena_winner

      # Check data inserted
      character = Characters.get!(character.id)

      assert character.class == attrs.class
      assert character.faction == attrs.faction
      assert character.gold == attrs.gold
      assert character.arena_winner == attrs.arena_winner
    end

    test "can create a character with items", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113,
        items: [
          item_attrs(%{slot: 0}),
          item_attrs(%{slot: 1}),
          item_attrs(%{slot: 2})
        ]
      }

      character =
        attrs
        |> Characters.create!()
        # We need to preload items first
        |> Characters.preload_items()

      assert length(character.items) == 3
    end

    test "must have a valid name", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: "test",
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: "foo"})
      end

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: "foofoofoofoofoo"})
      end

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: " spacebefore"})
      end

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: "spaceafter "})
      end

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: "space between"})
      end

      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: "おはよう"})
      end
    end

    test "name must be unique", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # First insert is fine
      _character = Characters.create!(attrs)

      # Same name raises an error
      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(attrs)
      end
    end

    @tag accounts: 2
    test "slot must be unique by account + slot", %{accounts: accounts} do
      [account1, account2] = accounts

      attrs = %{
        account: account1,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      # First insert is fine
      _character = Characters.create!(attrs)

      # Using same slot but another account is fine
      _character = Characters.create!(%{attrs | account: account2, name: random_string()})

      # Using same account but another slot is fine
      _character = Characters.create!(%{attrs | slot: 2, name: random_string()})

      # Same account + slot raise an error
      assert_raise Ecto.InvalidChangesetError, fn ->
        Characters.create!(%{attrs | name: random_string()})
      end
    end
  end

  describe "get/1" do
    test "get character by id", %{accounts: [account]} do
      %Character{name: name} =
        character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert {:ok,
              %Character{
                name: ^name,
                slot: 0,
                gender: :female,
                hair_color: :dark_purple,
                hair_style: :hair_style_a,
                map_id: 1,
                map_x: 77,
                map_y: 113
              }} = Characters.get(character.id)

      assert Characters.get(-1) == {:error, :not_found}
    end
  end

  describe "get!/1" do
    test "get character by id", %{accounts: [account]} do
      %Character{name: name} =
        character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert %Character{
               name: ^name,
               slot: 0,
               gender: :female,
               hair_color: :dark_purple,
               hair_style: :hair_style_a,
               map_id: 1,
               map_x: 77,
               map_y: 113
             } = Characters.get!(character.id)

      assert_raise Ecto.NoResultsError, fn ->
        Characters.get!(-1)
      end
    end
  end

  describe "get_by_account_and_slot/2" do
    test "return the character by account and slot", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      character = Characters.create!(attrs)

      # Fetch all attributes from database
      db_character = Characters.get!(character.id)

      assert Characters.get_by_account_and_slot(account, 0) == {:ok, db_character}
      assert Characters.get_by_account_and_slot(account, 1) == {:error, :not_found}
    end

    test "return the character by account_id and slot", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      character = Characters.create!(attrs)

      # Fetch all attributes from database
      db_character = Characters.get!(character.id)

      assert Characters.get_by_account_and_slot(account.id, 0) == {:ok, db_character}
      assert Characters.get_by_account_and_slot(account.id, 1) == {:error, :not_found}
    end
  end

  describe "list_by_account/2" do
    test "return the character list by account", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      character = Characters.create!(attrs)

      # Fetch all attributes from database
      db_character = Characters.get!(character.id)

      assert Characters.list_by_account(account) == [db_character]
    end

    test "return the character list by account_id", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      character = Characters.create!(attrs)

      # Fetch all attributes from database
      db_character = Characters.get!(character.id)

      assert Characters.list_by_account(account.id) == [db_character]
      assert Characters.list_by_account(-1) == []
    end
  end

  describe "preload_items/1" do
    test "return a list of items", %{accounts: [account]} do
      character1 =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      character2 =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 1,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113,
          items: [
            item_attrs(%{slot: 0}),
            item_attrs(%{slot: 1}),
            item_attrs(%{slot: 2})
          ]
        })

      # Get without preload
      character1 = Characters.get!(character1.id)
      character2 = Characters.get!(character2.id)

      assert %Ecto.Association.NotLoaded{} = character1.items
      assert %Character{} = preloaded_character1 = Characters.preload_items(character1)
      assert preloaded_character1.items == []

      assert %Ecto.Association.NotLoaded{} = character2.items
      assert %Character{} = preloaded_character2 = Characters.preload_items(character2)
      assert [%Item{}, %Item{}, %Item{}] = preloaded_character2.items
    end
  end

  describe "update/2" do
    test "update a character", %{accounts: [account]} do
      character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert Characters.update(character, %{gender: :male}) ==
               {:ok, %Character{character | gender: :male}}
    end
  end

  describe "update!/2" do
    test "update a character", %{accounts: [account]} do
      character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert Characters.update!(character, %{gender: :male}) ==
               %Character{character | gender: :male}
    end
  end

  describe "delete/1" do
    test "delete a character", %{accounts: [account]} do
      character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert {:ok, %Character{}} = Characters.delete(character)
      assert Characters.get(character.id) == {:error, :not_found}
    end
  end

  describe "delete!/1" do
    test "delete a character", %{accounts: [account]} do
      character =
        Characters.create!(%{
          account: account,
          name: random_string(),
          slot: 0,
          gender: :female,
          hair_color: :dark_purple,
          hair_style: :hair_style_a,
          map_id: 1,
          map_x: 77,
          map_y: 113
        })

      assert %Character{} = Characters.delete!(character)
      assert Characters.get(character.id) == {:error, :not_found}
    end
  end

  describe "list_deleted_characters_by_account/1" do
    test "return the deleted characters list by account", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert [] = Characters.list_deleted_characters_by_account(account)

      character1 = Characters.create!(attrs)
      %Character{id: character1_id} = Characters.delete!(character1)

      # Create a Character with the same slot + name
      # The first one is deleted so it should work
      _character2 = Characters.create!(attrs)

      assert [%Character{id: ^character1_id}] =
               Characters.list_deleted_characters_by_account(account)
    end

    test "return the deleted characters list by account_id", %{accounts: [account]} do
      attrs = %{
        account: account,
        name: random_string(),
        slot: 0,
        gender: :female,
        hair_color: :dark_purple,
        hair_style: :hair_style_a,
        map_id: 1,
        map_x: 77,
        map_y: 113
      }

      assert [] = Characters.list_deleted_characters_by_account(account.id)

      character1 = Characters.create!(attrs)
      %Character{id: character1_id} = Characters.delete!(character1)

      # Create a Character with the same slot + name
      # The first one is deleted so it should work
      _character2 = Characters.create!(attrs)

      assert [%Character{id: ^character1_id}] =
               Characters.list_deleted_characters_by_account(account.id)
    end
  end
end
