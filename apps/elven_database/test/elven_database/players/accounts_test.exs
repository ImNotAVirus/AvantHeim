defmodule ElvenDatabase.Players.AccountsTest do
  use ElvenDatabase.RepoCase, async: true

  alias ElvenDatabase.Players.{Account, Accounts, Character}

  ## Tests

  describe "create/1" do
    test "can create an account with raw password" do
      attrs = %{
        username: random_string(),
        password: random_string()
      }

      # Check structure returned by create/1
      assert {:ok, account} = Accounts.create(attrs)
      assert %Account{} = account
      assert account.username == attrs.username
      # Password is automatically deleted from struct
      assert account.password == nil
      assert account.hashed_password == hash(attrs.password)
      assert account.authority == nil
      assert account.language == nil
      assert %Ecto.Association.NotLoaded{} = account.characters

      # Check data inserted 
      account = Accounts.get!(account.id)

      assert account.username == attrs.username
      # Password shouln't be persisted
      assert account.password == nil
      assert account.hashed_password == hash(attrs.password)
      # Default to :player authority
      assert account.authority == :player
      # Default to :en language
      assert account.language == :en
      assert %Ecto.Association.NotLoaded{} = account.characters
    end

    test "can create an account with hashed password" do
      attrs = %{
        username: random_string(),
        hashed_password: hash(random_string())
      }

      assert {:ok, account} = Accounts.create(attrs)
      assert account.password == nil
      assert account.hashed_password == attrs.hashed_password
    end

    test "can create an account with authority" do
      attrs = %{
        username: random_string(),
        password: random_string(),
        authority: :administrator
      }

      assert {:ok, account} = Accounts.create(attrs)
      assert account.authority == attrs.authority
    end

    test "can create an account with language" do
      attrs = %{
        username: random_string(),
        password: random_string(),
        language: :fr
      }

      assert {:ok, account} = Accounts.create(attrs)
      assert account.language == attrs.language
    end

    test "can create an account with characters" do
      attrs = %{
        username: random_string(),
        hashed_password: hash(random_string()),
        characters: [
          character_attrs(%{slot: 0}),
          character_attrs(%{slot: 1})
        ]
      }

      assert {:ok, account} = Accounts.create(attrs)

      # We need to preload characters first
      account = Accounts.preload_characters(account)

      assert length(account.characters) == 2
    end

    test "username is required" do
      attrs = %{password: random_string()}

      assert {:error, changeset} = Accounts.create(attrs)
      assert changeset_error(changeset) == "username can't be blank"
    end

    test "password is required" do
      attrs = %{username: random_string()}

      assert {:error, changeset} = Accounts.create(attrs)
      assert changeset_error(changeset) == "hashed_password can't be blank"
    end

    test "username must be unique" do
      attrs = %{
        username: random_string(),
        password: random_string()
      }

      # First insert is fine
      assert {:ok, _account} = Accounts.create(attrs)

      # Same username returns an error
      assert {:error, changeset} = Accounts.create(attrs)
      assert changeset_error(changeset) == "username has already been taken"
    end
  end

  describe "create!/1" do
    test "can create an account with raw password" do
      attrs = %{
        username: random_string(),
        password: random_string()
      }

      # Check structure returned by create/1
      assert %Account{} = account = Accounts.create!(attrs)
      assert account.username == attrs.username
      # Password is automatically deleted from struct
      assert account.password == nil
      assert account.hashed_password == hash(attrs.password)
      assert account.authority == nil
      assert account.language == nil
      assert %Ecto.Association.NotLoaded{} = account.characters

      # Check data inserted 
      account = Accounts.get!(account.id)

      assert account.username == attrs.username
      # Password shouldn't be persisted
      assert account.password == nil
      assert account.hashed_password == hash(attrs.password)
      # Default to :player authority
      assert account.authority == :player
      # Default to :en language
      assert account.language == :en
      assert %Ecto.Association.NotLoaded{} = account.characters
    end

    test "can create an account with hashed password" do
      attrs = %{
        username: random_string(),
        hashed_password: hash(random_string())
      }

      account = Accounts.create!(attrs)
      assert account.password == nil
      assert account.hashed_password == attrs.hashed_password
    end

    test "can create an account with authority" do
      attrs = %{
        username: random_string(),
        password: random_string(),
        authority: :administrator
      }

      account = Accounts.create!(attrs)
      assert account.authority == attrs.authority
    end

    test "can create an account with language" do
      attrs = %{
        username: random_string(),
        password: random_string(),
        language: :fr
      }

      account = Accounts.create!(attrs)
      assert account.language == attrs.language
    end

    test "username is required" do
      attrs = %{password: random_string()}

      assert_raise Ecto.InvalidChangesetError, fn ->
        Accounts.create!(attrs)
      end
    end

    test "password is required" do
      attrs = %{username: random_string()}

      assert_raise Ecto.InvalidChangesetError, fn ->
        Accounts.create!(attrs)
      end
    end

    test "username must be unique" do
      attrs = %{
        username: random_string(),
        password: random_string()
      }

      # First insert is fine
      _account = Accounts.create!(attrs)

      # Same username returns an error
      assert_raise Ecto.InvalidChangesetError, fn ->
        Accounts.create!(attrs)
      end
    end
  end

  describe "get/1" do
    test "get account by id" do
      account =
        Accounts.create!(%{
          username: random_string(),
          password: random_string(),
          authority: :game_master,
          language: :fr
        })

      assert Accounts.get(account.id) == {:ok, account}
      assert Accounts.get(-1) == {:error, :not_found}
    end
  end

  describe "get!/1" do
    test "get account by id" do
      account =
        Accounts.create!(%{
          username: random_string(),
          password: random_string(),
          authority: :game_master,
          language: :fr
        })

      assert Accounts.get!(account.id) == account

      assert_raise Ecto.NoResultsError, fn ->
        Accounts.get!(-1)
      end
    end
  end

  describe "authenticate/2" do
    test "get account by username and hashed_password" do
      account =
        Accounts.create!(%{
          username: random_string(),
          password: random_string(),
          authority: :game_master,
          language: :fr
        })

      assert Accounts.authenticate(account.username, account.hashed_password) == {:ok, account}
    end
  end

  describe "preload_characters/1" do
    test "return a list of characters" do
      account1 =
        Accounts.create!(%{
          username: random_string(),
          password: random_string(),
          authority: :player,
          language: :en
        })

      account2 =
        Accounts.create!(%{
          username: random_string(),
          password: random_string(),
          authority: :game_master,
          language: :fr,
          characters: [
            character_attrs(%{slot: 0}),
            character_attrs(%{slot: 1})
          ]
        })

      # Get without preload
      account1 = Accounts.get!(account1.id)
      account2 = Accounts.get!(account2.id)

      assert %Ecto.Association.NotLoaded{} = account1.characters
      assert %Account{} = preloaded_account1 = Accounts.preload_characters(account1)
      assert preloaded_account1.characters == []

      assert %Ecto.Association.NotLoaded{} = account2.characters
      assert %Account{} = preloaded_account2 = Accounts.preload_characters(account2)
      assert [%Character{}, %Character{}] = preloaded_account2.characters
    end
  end

  ## Private functions

  defp hash(password) do
    password
    |> then(&:crypto.hash(:sha512, &1))
    |> Base.encode16()
  end
end
