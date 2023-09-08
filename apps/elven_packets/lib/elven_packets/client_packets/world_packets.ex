defmodule ElvenPackets.Client.WorldPackets do
  @moduledoc """
  TODO: ElvenPackets.Client.WorldPackets
  """

  import ElvenGard.Network.PacketSerializer, only: [import_packets: 1]

  import_packets ElvenPackets.Client.AreaPackets
  import_packets ElvenPackets.Client.LobbyPackets
end
