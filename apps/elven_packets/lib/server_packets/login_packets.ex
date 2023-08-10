defmodule ElvenPacket.Server.LoginPackets do
  @moduledoc """
  TODO: ElvenPacket.Server.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Server.LoginEnums, only: [failc_error: 1]

  alias MinecraftEx.Types.Enum

  ## Login packets

  @serializable true
  defpacket "failc", as: Failc do
    field :error, Enum, default: :generic, values: failc_error(:__enumerators__)
  end
end
