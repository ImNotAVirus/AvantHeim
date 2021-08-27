defmodule ChannelEndpoint.Endpoint.ShopViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ChannelEndpoint.Endpoint.ShopPackets.Buy
  alias CachingService.Player.Character

  import ChannelEndpoint.Endpoint.ShopPackets.BuyEnums, only: [shop_type: 1]

  ## Public API

  @spec render(atom, any) :: any
  def render(:buy, %{entity: %Character{id: id}, slot: slot, amount: amount}) do
    %Buy{
      shop_type: shop_type(:character_shop),
      entity_id: id,
      slot: slot,
      amount: amount
    }
  end
end
