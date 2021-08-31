defmodule ChannelEndpoint.GroupRequestEnums do
  @moduledoc """
  TODO: Documentation.
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :group_request_type,
    requested: 0,
    invited: 1,
    accepted: 3,
    declined: 4,
    sharing: 5,
    accepted_share: 6,
    declined_share: 7
end
