defmodule ElvenPackets.Server.ChatPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.ChatPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenPackets.Enums.ChatEnums, only: [color_type: 1]
  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  alias ElvenPackets.Types.{NsEnum, NsInteger, NsString}

  @serializable true
  defpacket "bn", as: Bn do
    field :id, NsInteger
    field :message, NsString, escape: true
  end

  @serializable true
  defpacket "say", as: Say do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :color, NsEnum, default: :default, values: color_type(:__enumerators__)
    field :message, NsString
  end
end
