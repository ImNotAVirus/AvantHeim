defmodule ElvenViews.UIPackets.BankEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :action_type, [
    :open_from_savings_book,
    :deposit_gold,
    :withdraw_gold,
    :open_bank
  ]

  defenum :text_color,
    green: 4,
    red: 5,
    white: 6
end
