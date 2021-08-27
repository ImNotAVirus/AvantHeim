defmodule ChannelEndpoint.Endpoint.ShopViews do
  @moduledoc """
  TODO: Documentation
  """

  alias CachingService.Player.Character
  alias ChannelEndpoint.Endpoint.ShopPackets.Buy

  ## Public API

  @spec render(atom, any) :: any
  def render(:buy, %{shop_type: shop_type, entity_id: entity_id, slot: slot, amount: amount}) do
    %Buy{
      shop_type: shop_type,
      entity_id: entity_id,
      slot: slot,
      amount: amount
    }
  end
end
