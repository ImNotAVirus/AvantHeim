defmodule ElvenViews.UIPackets.CancelEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :cancel_type,
    auto_attack: 0,
    skill: 2
end
