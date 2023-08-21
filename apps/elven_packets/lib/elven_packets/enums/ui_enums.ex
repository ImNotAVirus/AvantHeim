defmodule ElvenPackets.Enums.UIEnums do
  @moduledoc """
  TODO: ElvenPackets.Enums.UIEnums
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

  defenum :cancel_type,
    # Casting is the status after using an skill.
    # The client waits until x packet/s appear, until then
    # it blocks the movement)
    skill: 0,
    # Needs to be verified
    picking: 1,
    # On NosCore it's called `auto_attack`. But I'm pretty sure this
    # enum is not only used for AA. So I called it `action`
    # Changes AutoAttack to Selected. AutoAttack = Red arrow on top of
    # the enemy. Selected = Yellow arrow.
    action: 2
end
