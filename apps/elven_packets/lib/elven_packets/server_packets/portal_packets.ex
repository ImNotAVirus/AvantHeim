defmodule ElvenPackets.Server.PortalPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.PortalPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenData.Enums.MapEnums, only: [portal_direction_type: 1]

  alias ElvenPackets.Types.{NsEnum, NsInteger, NsBoolean}

  #######
  # Portal display
  # ---
  # Example: "gp 123 456 1 -1 1 0"
  #######
  @serializable true
  defpacket "gp", as: Gp do
    field :source_x, NsInteger
    field :source_y, NsInteger
    field :map_id, NsInteger
    field :portal_type, NsInteger
    field :portal_direction, NsEnum, values: portal_direction_type(:__enumerators__)
    field :is_disabled, NsBoolean
  end
end
