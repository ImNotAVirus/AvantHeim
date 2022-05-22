defmodule ElvenViews.LobbyPackets.ClistStartPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "clist_start" do
    field :unused, :integer, default: 0
  end
end
