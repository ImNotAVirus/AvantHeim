defmodule ElvenPackets.Server.ChatPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.ChatPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenEnums.EntityEnums, only: [direction_type: 1]

  alias ElvenPackets.Types.{NsEnum, NsInteger, NsString}

  @serializable true
  defpacket "at", as: At do
    field :character_id, NsInteger
    field :map_vnum, NsInteger
    field :map_x, NsInteger
    field :map_y, NsInteger
    field :direction, NsEnum, values: direction_type(:__enumerators__)
    field :unknown1, default: 0, NsInteger
    field :map_music, NsInteger
    field :unknown2, default: -2, NsInteger
    field :unknown3, default: -1, NsInteger
  end

  @serializable true
  defpacket "c_map", as: Cmap do
    # Always 0, idk
    field :type, :default: 0, NsInteger
    field :map_vnum, NsInteger
    field :is_static_map, NsInteger
  end

  @serializable true
  defpacket "mapout", as: Mapout

  @serializable true
  defpacket "mv" do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :map_x, NsInteger
    field :map_y, NsInteger
    field :speed, NsInteger
  end
end
