defmodule ElvenViews.UIPackets.GoldPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "gold" do
    field :gold, :non_neg_integer
    field :bank_gold, :non_neg_integer
  end
end
