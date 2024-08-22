defmodule ElvenDatabase.Players.Account do
  @moduledoc """
  Holds information about an Account.
  """

  use Ecto.Schema

  import Ecto.Changeset

  require ElvenData.Enums.PlayerEnums, as: PlayerEnums

  alias __MODULE__
  alias ElvenDatabase.Players.Character

  @type id :: non_neg_integer()
  @type t :: %Account{
          id: id(),
          username: String.t(),
          password: String.t(),
          hashed_password: String.t(),
          authority: PlayerEnums.authority_keys(),
          language: PlayerEnums.language_keys(),
          characters: [Character.t()],
          # Ecto fields
          __meta__: Ecto.Schema.Metadata.t(),
          inserted_at: any(),
          updated_at: any()
        }

  ## Schema

  schema "accounts" do
    field :username, :string
    field :password, :string, virtual: true
    field :hashed_password, :string
    field :authority, Ecto.Enum, values: PlayerEnums.authority(:__keys__)
    field :language, Ecto.Enum, values: PlayerEnums.language(:__keys__)

    has_many :characters, Character

    timestamps()
  end

  ## Public API

  @fields [:username, :password, :hashed_password, :authority, :language]

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(account, attrs) do
    account
    |> cast(attrs, @fields)
    |> cast_assoc(:characters, with: &Character.assoc_changeset/2)
    |> unique_constraint(:username)
    |> maybe_hash_password()
    |> validate_required([:username, :hashed_password])
  end

  ## Private functions

  defp maybe_hash_password(changeset) do
    password = get_change(changeset, :password)
    hashed_password = get_change(changeset, :hashed_password)

    if changeset.valid?() and not is_nil(password) and is_nil(hashed_password) do
      new_password = password |> then(&:crypto.hash(:sha512, &1)) |> Base.encode16()

      changeset
      |> put_change(:hashed_password, new_password)
      |> delete_change(:password)
    else
      changeset
    end
  end
end
