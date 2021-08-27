defmodule ChannelEndpoint.Endpoint.ShopPackets.BuyEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :shop_type,
    character_shop: 1,
    item_shop: 2
end
