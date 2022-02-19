defmodule ElvenViews.MapPackets.MvPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  ## Packet definition

  defpacket "mv" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :map_x, :non_neg_integer
    field :map_y, :non_neg_integer
    field :speed, :non_neg_integer
  end
end
