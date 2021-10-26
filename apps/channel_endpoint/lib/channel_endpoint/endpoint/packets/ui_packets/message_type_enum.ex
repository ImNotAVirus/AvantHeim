defmodule ChannelEndpoint.Endpoint.UIPackets.MessageTypeEnum do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :message_type,
    whisper: 0,
    private_chat: 1,
    shout: 2,
    white: 3,
    family_chat: 4
end
