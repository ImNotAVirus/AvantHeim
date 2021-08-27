defmodule ChannelEndpoint.Endpoint.ShopPackets.BuyEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :shop_type,
    charactershop: 1,
    itemshop: 2
end
