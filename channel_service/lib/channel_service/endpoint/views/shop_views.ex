defmodule ChannelService.Endpoint.ShopViews do
  @moduledoc """
  TODO: Documentation
  """

  alias ChannelService.Endpoint.ShopPackets.Buy
  alias CachingService.Player.Character

  ## Public API

  @spec render(atom, any) :: any
  def render(:buy, %{entity: %Character{id: id}, slot: slot, amount: amount}) do
    %Buy{
      shop_type: :character_shop,
      entity_id: id,
      slot: slot,
      amount: amount
    }
  end
end
