defmodule DatabaseService.Players.Account do
  @moduledoc """
  TODO: Documentation
  """

  use Ecto.Schema

  schema "accounts" do
    field :username, :string
    field :password, :string
    field :authority, DatabaseService.EctoAuthority
    field :language, Ecto.Enum, values: [:en, :fr]

    timestamps()
  end
end
