defmodule ChannelEndpoint.MapEnums do
  @moduledoc """
  TODO: Documentation
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :spawn_effect_type, [
    :summon,
    :no_effect,
    :falling
  ]
end
