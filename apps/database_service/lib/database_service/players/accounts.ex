defmodule DatabaseService.Players.Accounts do
  @moduledoc """
  TODO: Documentation
  """

  alias DatabaseService.Players.Account
  alias DatabaseService.Repo

  @spec create(map) :: {:ok, Ecto.Schema.t()} | {:error, Ecto.Changeset.t()}
  def create(attrs) do
    %Account{}
    |> Account.changeset(attrs)
    |> Repo.insert()
  end

  @spec create!(map) :: Ecto.Schema.t()
  def create!(attrs) do
    %Account{}
    |> Account.changeset(attrs)
    |> Repo.insert!()
  end
end
