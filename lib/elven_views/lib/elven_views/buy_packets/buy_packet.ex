defmodule ElvenViews.ShopPackets.BuyPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.ShopPackets.BuyEnums, only: [shop_type: 1]

  ## Packet definition

  defpacket "buy" do
    field :shop_type, :enum, values: shop_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :slot, :non_neg_integer
    field :amount, :pos_integer
  end
end
