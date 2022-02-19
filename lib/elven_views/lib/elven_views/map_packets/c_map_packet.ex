defmodule ElvenViews.MapPackets.CMapPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "c_map" do
    field :type, :non_neg_integer, default: 0
    field :map_vnum, :pos_integer
    field :is_static_map, :boolean
  end
end
