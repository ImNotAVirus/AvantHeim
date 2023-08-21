defmodule ElvenViews.SubPackets.EquipmentSubPacket do
  @moduledoc false

  use ElvenCore.SerializableStruct

  alias __MODULE__

  @enforce_keys [
    :hat,
    :armor,
    :main_weapon,
    :secondary_weapon,
    :mask,
    :fairy,
    :costume_suit,
    :costume_hat,
    :weapon_skin,
    :wings_skin
  ]

  defstruct @enforce_keys

  @type t :: %EquipmentSubPacket{
          hat: non_neg_integer,
          armor: non_neg_integer,
          main_weapon: non_neg_integer,
          secondary_weapon: non_neg_integer,
          mask: non_neg_integer,
          fairy: non_neg_integer,
          costume_suit: non_neg_integer,
          costume_hat: non_neg_integer,
          weapon_skin: non_neg_integer,
          wings_skin: non_neg_integer
        }

  ## SerializableStruct behaviour

  @impl true
  def serialize(%EquipmentSubPacket{} = struct, _) do
    serialize_term(
      [
        struct.hat,
        struct.armor,
        struct.main_weapon,
        struct.secondary_weapon,
        struct.mask,
        struct.fairy,
        struct.costume_suit,
        struct.costume_hat,
        struct.weapon_skin,
        struct.wings_skin
      ],
      as: :integer,
      joiner: "."
    )
  end
end
