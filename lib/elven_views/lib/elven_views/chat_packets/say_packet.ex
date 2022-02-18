defmodule ElvenViews.ChatPackets.SayPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]
  import ElvenViews.ChatPackets.SayEnums, only: [color_type: 1]

  ## Packet definition

  defpacket "say" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :color, :enum, values: color_type(:__enumerators__)
    field :message, :string
  end
end
