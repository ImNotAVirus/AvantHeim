defmodule ElvenDatabase.Players.Accounts do
  @moduledoc """
  Module for querying Accounts information from the database.
  """

  alias ElvenDatabase.Players.Account
  alias ElvenDatabase.Repo

  # Dyalizer doesn't like `Account.changeset(%Account{}, attrs)`
  # because fields on Account struct can't be nil
  @dialyzer [
    {:no_return, create: 1, create!: 1},
    {:no_fail_call, create: 1, create!: 1}
  ]

  ## Public API

  @spec authenticate(String.t(), String.t()) :: {:ok, Account.t()} | {:error, :not_found}
  def authenticate(username, hashed_password) do
    case Repo.get_by(Account, username: username, hashed_password: hashed_password) do
      %Account{} = account -> {:ok, account}
      nil -> {:error, :not_found}
    end
  end

  @spec create(map()) :: {:ok, Account.t()} | {:error, Ecto.Changeset.t()}
  def create(attrs) do
    %Account{}
    |> Account.changeset(attrs)
    |> Repo.insert()
  end

  @spec create!(map()) :: Account.t()
  def create!(attrs) do
    %Account{}
    |> Account.changeset(attrs)
    |> Repo.insert!()
  end

  @spec get(Account.id()) :: {:ok, Account.t()} | {:error, :not_found}
  def get(id) do
    case Repo.get(Account, id) do
      nil -> {:error, :not_found}
      item -> {:ok, item}
    end
  end

  @spec get!(Account.id()) :: Account.t()
  def get!(id) do
    Repo.get!(Account, id)
  end

  @spec preload_characters(Account.t()) :: Account.t()
  def preload_characters(account) do
    Repo.preload(account, :characters)
  end
end
