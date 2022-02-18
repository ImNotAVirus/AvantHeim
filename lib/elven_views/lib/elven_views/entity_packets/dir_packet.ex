defmodule ElvenViews.EntityPackets.DirPacket do
  @moduledoc """
  Player direction
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1, direction_type: 1]

  ## Packet definition

  defpacket "dir" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :direction_type, :enum, values: direction_type(:__enumerators__)
  end
end
