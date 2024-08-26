defmodule ElvenDatabase.Players.Characters do
  @moduledoc """
  Module for querying Characters information from the database.
  """

  import Ecto.Query, only: [from: 2]

  alias ElvenDatabase.Players.{Account, Character}
  alias ElvenDatabase.Repo

  # Dyalizer doesn't like `Character.changeset(%Character{}, attrs)`
  # because fields on Character struct can't be nil
  @dialyzer [
    {:no_return, create: 1, create!: 1},
    {:no_fail_call, create: 1, create!: 1}
  ]

  @typep account :: Account.t() | Account.id()

  ## Public API

  @spec create(map()) :: {:ok, Character.t()} | {:error, Ecto.Changeset.t()}
  def create(attrs) do
    %Character{}
    |> Character.changeset(attrs)
    |> Repo.insert()
  end

  @spec create!(map()) :: Character.t()
  def create!(attrs) do
    %Character{}
    |> Character.changeset(attrs)
    |> Repo.insert!()
  end

  @spec get(Character.id()) :: {:ok, Character.t()} | {:error, :not_found}
  def get(id) do
    case Repo.get(Character, id) do
      nil -> {:error, :not_found}
      item -> {:ok, item}
    end
  end

  @spec get!(Character.id()) :: Character.t()
  def get!(id) do
    Repo.get!(Character, id)
  end

  @spec get_by_account_and_slot(account(), Character.slot()) ::
          {:ok, Character.t()} | {:error, :not_found}
  def get_by_account_and_slot(%Account{id: account_id}, slot) do
    get_by_account_and_slot(account_id, slot)
  end

  def get_by_account_and_slot(account_id, slot) do
    case Repo.get_by(Character, account_id: account_id, slot: slot) do
      %Character{} = character -> {:ok, character}
      nil -> {:error, :not_found}
    end
  end

  @spec list_by_account(account()) :: [Character.t()]
  def list_by_account(%Account{id: account_id}) do
    list_by_account(account_id)
  end

  def list_by_account(account_id) do
    from(c in Character, where: c.account_id == ^account_id)
    |> Repo.all()
  end

  @spec preload_items(Character.t()) :: Character.t()
  def preload_items(account) do
    Repo.preload(account, :items)
  end
end
