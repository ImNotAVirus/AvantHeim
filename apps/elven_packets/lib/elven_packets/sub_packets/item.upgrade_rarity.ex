defmodule ElvenPackets.SubPackets.Item.UpgradeRarity do
  @moduledoc false

  use ElvenGard.Network.Type

  alias __MODULE__

  @enforce_keys [:upgrade, :rarity]
  defstruct @enforce_keys

  @type t :: %UpgradeRarity{upgrade: non_neg_integer(), rarity: non_neg_integer()}

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(%UpgradeRarity{} = struct, _) do
    %UpgradeRarity{upgrade: upgrade, rarity: rarity} = struct
    "#{upgrade}#{rarity}"
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
