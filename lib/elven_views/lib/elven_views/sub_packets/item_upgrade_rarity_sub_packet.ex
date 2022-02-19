defmodule ElvenViews.SubPackets.ItemUpgradeRaritySubPacket do
  @moduledoc false

  use ElvenCore.SerializableStruct

  alias __MODULE__

  @enforce_keys [:upgrade, :rarity]
  defstruct @enforce_keys

  @type t :: %ItemUpgradeRaritySubPacket{
          upgrade: non_neg_integer,
          rarity: non_neg_integer
        }

  ## SerializableStruct behaviour

  @impl true
  def serialize(%ItemUpgradeRaritySubPacket{} = struct, _) do
    "#{struct.upgrade}#{struct.rarity}"
  end
end
