defmodule ElvenViews.UIPackets.InfoPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "info" do
    field :message, :string
  end
end
