defmodule ChannelEndpoint.Endpoint.ShopPackets.Buy do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.ShopPackets.BuyEnums, only: [shop_type: 1]

  alias __MODULE__

  @enforce_keys [:shop_type, :entity_id, :slot, :amount]
  defstruct @enforce_keys

  @type t :: %Buy{
          shop_type: atom,
          entity_id: pos_integer,
          slot: non_neg_integer,
          amount: pos_integer
        }

  @impl true
  def serialize(%Buy{} = struct, _) do
    %Buy{
      shop_type: shop_type_atom,
      entity_id: entity_id,
      slot: slot,
      amount: amount
    } = struct

    ["buy", shop_type(shop_type_atom), entity_id, slot, amount]
  end
end
