defmodule ElvenPackets.Server.LoginPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.LoginEnums, only: [failc_error: 1]

  alias ElvenPackets.Types.NsEnum

  ## Login packets

  @serializable true
  defpacket "failc", as: Failc do
    field :error, NsEnum, default: :generic, values: failc_error(:__enumerators__)
  end
end
