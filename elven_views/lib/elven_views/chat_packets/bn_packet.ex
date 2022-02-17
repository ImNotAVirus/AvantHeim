defmodule ElvenViews.ChatPackets.BnPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "bn" do
    field :id, :pos_integer
    field :message, :string, escape: true
  end
end
