defmodule ElvenDatabase.Players.Accounts do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenDatabase.Players.Account
  alias ElvenDatabase.Repo

  @spec log_in(String.t(), String.t()) :: Ecto.Schema.t() | nil
  def log_in(username, hashed_password) do
    Repo.get_by(Account, username: username, hashed_password: hashed_password)
  end

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
