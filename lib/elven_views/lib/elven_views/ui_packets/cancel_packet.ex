defmodule ElvenViews.UIPackets.CancelPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.UIPackets.CancelEnums, only: [cancel_type: 1]

  ## Packet definition

  defpacket "cancel" do
    field :cancel_type, :enum, values: cancel_type(:__enumerators__)
    field :entity_id, :integer, default: 0
    field :unknown, :integer, default: -1
  end
end
