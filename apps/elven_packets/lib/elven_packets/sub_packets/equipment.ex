defmodule ElvenPackets.SubPackets.Equipment do
  @moduledoc false

  use ElvenGard.Network.Type

  alias __MODULE__
  alias ElvenPackets.Types.{NsInteger, NsList}

  defstruct [
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

  @type t :: %Equipment{
          hat: non_neg_integer | nil,
          armor: non_neg_integer | nil,
          main_weapon: non_neg_integer | nil,
          secondary_weapon: non_neg_integer | nil,
          mask: non_neg_integer | nil,
          fairy: non_neg_integer | nil,
          costume_suit: non_neg_integer | nil,
          costume_hat: non_neg_integer | nil,
          weapon_skin: non_neg_integer | nil,
          wings_skin: non_neg_integer | nil
        }

  ## ElvenGard.Network.Type behaviour

  @impl true
  @spec encode(t(), Keyword.t()) :: iodata()
  def encode(%Equipment{} = struct, _) do
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
    ]
    |> Enum.map(&(&1 || -1))
    |> NsList.encode(type: NsInteger, joiner: ".")
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
