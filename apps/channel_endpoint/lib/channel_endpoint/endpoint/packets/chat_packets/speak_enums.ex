defmodule ChannelEndpoint.Endpoint.ChatPackets.SpeakEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :speak_type,
    group: 3,
    player: 5,
    gamemaster: 15
end
