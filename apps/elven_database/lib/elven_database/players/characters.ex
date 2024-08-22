defmodule ElvenDatabase.Players.Characters do
  @moduledoc """
  Module for querying Characters information from the database.
  """

  import Ecto.Query, only: [from: 2]

  alias ElvenDatabase.Players.Character
  alias ElvenDatabase.Repo

  @spec all_by_account_id(pos_integer, boolean) :: [Character.t()]
  def all_by_account_id(account_id, include_disabled \\ false)

  def all_by_account_id(account_id, true) do
    from(c in Character, where: c.account_id == ^account_id)
    |> Repo.all()
  end

  def all_by_account_id(account_id, false) do
    from(c in Character, where: c.account_id == ^account_id and c.disabled == false)
    |> Repo.all()
  end

  @spec get_by_account_id_and_slot(pos_integer, non_neg_integer) :: Character.t() | nil
  def get_by_account_id_and_slot(account_id, slot) do
    from(a in Character,
      where:
        a.account_id == ^account_id and
          a.slot == ^slot and a.disabled == false
    )
    |> Repo.one()
  end

  @spec create(map) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def create(attrs) do
    %Character{}
    |> Character.changeset(attrs)
    |> Repo.insert()
  end

  @spec create!(map) :: Ecto.Schema.t()
  def create!(attrs) do
    %Character{}
    |> Character.changeset(attrs)
    |> Repo.insert!()
  end
end
