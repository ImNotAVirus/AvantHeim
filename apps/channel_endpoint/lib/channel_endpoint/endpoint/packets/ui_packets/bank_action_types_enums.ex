defmodule ChannelEndpoint.Endpoint.UIPackets.BankActionTypesEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :bank_action_type,
    open_from_savings_book: 0,
    deposit_gold: 1,
    withdraw_gold: 2,
    open_bank: 3
end
