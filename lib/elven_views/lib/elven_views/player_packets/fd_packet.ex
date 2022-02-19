defmodule ElvenViews.PlayerPackets.FdPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "fd" do
    field :reputation, :integer
    field :reputation_icon_id, :integer
    field :dignity, :integer
    field :dignity_icon_id, :integer
  end
end
