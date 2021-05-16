defmodule DatabaseService.Players.Characters do
  @moduledoc """
  TODO: Documentation
  """

  import Ecto.Query, only: [from: 2]

  alias DatabaseService.Players.Character
  alias DatabaseService.Repo

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
