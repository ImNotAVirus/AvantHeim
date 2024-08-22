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

  ## Public API

  @spec all_by_account_id(Account.id(), boolean()) :: [Character.t()]
  def all_by_account_id(account_id, include_disabled \\ false)

  def all_by_account_id(account_id, true) do
    from(c in Character, where: c.account_id == ^account_id)
    |> Repo.all()
  end

  def all_by_account_id(account_id, false) do
    from(c in Character, where: c.account_id == ^account_id and c.disabled == false)
    |> Repo.all()
  end

  @spec get_by_account_id_and_slot(Account.id(), Character.slot()) :: Character.t() | nil
  def get_by_account_id_and_slot(account_id, slot) do
    from(a in Character,
      where:
        a.account_id == ^account_id and
          a.slot == ^slot and a.disabled == false
    )
    |> Repo.one()
  end

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

  @spec preload_items(Character.t()) :: Character.t()
  def preload_items(account) do
    Repo.preload(account, :items)
  end
end
