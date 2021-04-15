defmodule DatabaseService.Players.Account do
  @moduledoc """
  TODO: Documentation
  """

  use Ecto.Schema

  import Ecto.Changeset

  ## Schema

  schema "accounts" do
    field :username, :string
    field :password, :string, virtual: true
    field :hashed_password, :string
    field :authority, DatabaseService.EctoAuthority
    field :language, Ecto.Enum, values: [:en, :fr]

    timestamps()
  end

  ## Public API

  def changeset(account, attrs) do
    account
    |> cast(attrs, [:username, :password, :authority, :language])
    |> unique_constraint(:username)
    |> maybe_hash_password()
    |> validate_required([:username, :hashed_password])
  end

  ## Private functions

  defp maybe_hash_password(changeset) do
    password = get_change(changeset, :password)
    hashed_password = get_change(changeset, :hashed_password)

    if changeset.valid?() && password && is_nil(hashed_password) do
      new_password = :sha512 |> :crypto.hash(password) |> Base.encode16()

      changeset
      |> put_change(:hashed_password, new_password)
      |> delete_change(:password)
    else
      changeset
    end
  end
end
