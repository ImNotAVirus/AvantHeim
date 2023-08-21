defmodule ElvenViews.VisibilityPackets.OutPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  ## Packet definition

  defpacket "out" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
  end
end
