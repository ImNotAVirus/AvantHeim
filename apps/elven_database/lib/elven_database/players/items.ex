defmodule ElvenDatabase.Players.Items do
  @moduledoc """
  TODO: Documentation
  """

  import Ecto.Query, only: [from: 2]

  alias ElvenDatabase.Players.Item
  alias ElvenDatabase.Repo

  # Dyalizer doesn't like `Item.changeset(%Item{}, attrs)`
  # because fields on Item struct can't be nil
  @dialyzer [
    {:no_return, create: 1, create!: 1},
    {:no_fail_call, create: 1, create!: 1}
  ]

  ## Public API

  @spec create(map()) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def create(attrs) do
    %Item{}
    |> Item.changeset(attrs)
    |> Repo.insert()
  end

  @spec create!(map()) :: Ecto.Schema.t()
  def create!(attrs) do
    %Item{}
    |> Item.changeset(attrs)
    |> Repo.insert!()
  end

  @spec list_by_owner(non_neg_integer()) :: [Item.t()]
  def list_by_owner(character_id) do
    from(c in Item, where: c.owner_id == ^character_id)
    |> Repo.all()
  end

  @spec update(Item.t(), map()) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def update(%Item{} = item, attrs) do
    item
    |> Item.changeset(attrs)
    |> Repo.update()
  end

  @spec update!(Item.t(), map()) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def update!(%Item{} = item, attrs) do
    item
    |> Item.changeset(attrs)
    |> Repo.update!()
  end

  @spec delete(Item.t()) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def delete(%Item{} = item) do
    Repo.delete(item)
  end

  @spec delete!(Item.t()) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def delete!(%Item{} = item) do
    Repo.delete!(item)
  end
end
