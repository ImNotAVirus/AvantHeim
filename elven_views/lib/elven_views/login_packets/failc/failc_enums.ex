defmodule ElvenViews.LoginPackets.FailcEnums do
  @doc false

  import SimpleEnum, only: [defenum: 2]

  defenum :error, old_client: 1, generic: 2, already_connected: 4, bad_credentials: 5
end
