defmodule ElvenDatabase.Players.Items do
  @moduledoc """
  TODO: Documentation
  """

  # import Ecto.Query, only: [from: 2]

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
end
