defmodule ElvenDatabase.Players.CharactersTest do
  use ElvenDatabase.RepoCase, async: true

  alias ElvenDatabase.Players.{Accounts, Character, Characters}

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
end
