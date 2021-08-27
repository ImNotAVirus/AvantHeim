defmodule ChannelEndpoint.Endpoint.ShopPackets.BuyEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum(:shop_type,
    CharacterShop: 1,
    ItemShop: 2
  )
end
