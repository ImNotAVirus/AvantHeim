defmodule ElvenDatabase.Players.Items do
  @moduledoc """
  TODO: Documentation
  """

  # import Ecto.Query, only: [from: 2]

  alias ElvenDatabase.Players.Item
  alias ElvenDatabase.Repo

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
