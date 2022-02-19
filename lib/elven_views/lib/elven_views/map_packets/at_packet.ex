defmodule ElvenViews.MapPackets.AtPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [direction_type: 1]

  ## Packet definition

  defpacket "at" do
    field :character_id, :pos_integer
    field :map_vnum, :pos_integer
    field :map_x, :non_neg_integer
    field :map_y, :non_neg_integer
    field :direction, :enum, values: direction_type(:__enumerators__)
    field :unknown1, :integer, default: 0
    field :map_music, :non_neg_integer
    field :unknown2, :integer, default: 2
    field :unknown3, :integer, default: -1
  end
end
