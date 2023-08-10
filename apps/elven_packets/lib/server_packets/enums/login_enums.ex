defmodule ElvenPackets.Server.LoginEnums do
  @moduledoc """
  TODO: ElvenPackets.Server.LoginEnums
  """

  import SimpleEnum, only: [defenum: 2]

  defenum :failc_error, old_client: 1, generic: 2, already_connected: 4, bad_credentials: 5
end
