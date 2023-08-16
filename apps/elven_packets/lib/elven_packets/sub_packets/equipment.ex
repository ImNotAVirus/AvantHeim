defmodule ElvenPackets.SubPackets.Equipment do
  @moduledoc false

  use ElvenGard.Network.Type

  alias __MODULE__
  alias ElvenPackets.Types.{NsInteger, NsList}

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

  @type t :: %Equipment{
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

  ## ElvenGard.Network.Type behaviour

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(%Equipment{} = struct, _) do
    NsList.encode(
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
      type: NsInteger,
      joiner: "."
    )
  end

  @impl true
  def decode(_data, _opts) do
    # We do this to trick Dialyzer to not complain about non-local returns.
    case :erlang.phash2(1, 1) do
      0 -> raise("unimplemented decoder for #{inspect(__MODULE__)}")
      1 -> {nil, ""}
    end
  end
end
