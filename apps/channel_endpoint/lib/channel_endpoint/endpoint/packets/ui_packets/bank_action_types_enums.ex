defmodule ChannelEndpoint.Endpoint.UIPackets.BankActionTypesEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :bank_action_type, [
    :open_from_savings_book,
    :deposit_gold,
    :withdraw_gold,
    :open_bank
  ]
end
