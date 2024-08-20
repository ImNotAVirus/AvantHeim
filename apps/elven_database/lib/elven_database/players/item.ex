defmodule ElvenDatabase.Players.Item do
  @moduledoc false

  use Ecto.Schema

  import Ecto.Changeset

  require ElvenData.Enums.ItemEnums, as: ItemEnums

  alias __MODULE__

  @type t :: %Item{
          id: non_neg_integer(),
          owner_id: non_neg_integer(),
          inventory_type: ItemEnums.inventory_type_keys(),
          slot: ItemEnums.slot_type() | non_neg_integer(),
          vnum: non_neg_integer(),
          quantity: non_neg_integer(),
          # Ecto fields
          __meta__: Ecto.Schema.Metadata.t(),
          inserted_at: any(),
          updated_at: any()
        }

  ## Schema

  schema "items" do
    belongs_to :owner, ElvenDatabase.Players.Character

    field :inventory_type, Ecto.Enum, values: ItemEnums.inventory_type(:__keys__)
    field :slot, :integer
    field :vnum, :integer
    field :quantity, :integer

    timestamps()
  end

  ## Public API

  @fields [
    :owner_id,
    :inventory_type,
    :slot,
    :vnum,
    :quantity
  ]

  @spec changeset(Item.t(), map()) :: Ecto.Changeset.t()
  def changeset(%Item{} = item, attrs) do
    attrs =
      case attrs do
        %{slot: slot} when is_atom(slot) -> Map.put(attrs, :slot, ItemEnums.slot_type(slot))
        attrs -> attrs
      end

    item
    |> cast(attrs, @fields)
    |> cast_assoc(:owner)
    |> validate_required(@fields)
    |> unique_constraint(:slot, name: :owner_inventory_slot)
  end
end
