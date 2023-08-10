defmodule ElvenPackets.Enums.LoginEnums do
  @moduledoc """
  TODO: ElvenPackets.Enums.LoginEnums
  """

  import SimpleEnum, only: [defenum: 2]

  ## Login (NoS0575, NoS0577, NsTeST)

  defenum :login_region, [:en, :de, :fr, :it, :pl, :es, :cz, :ru, :tr]

  ## failc
  defenum :failc_error, old_client: 1, generic: 2, already_connected: 4, bad_credentials: 5
end
