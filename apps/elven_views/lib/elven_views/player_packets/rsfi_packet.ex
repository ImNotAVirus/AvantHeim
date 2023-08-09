defmodule ElvenViews.PlayerPackets.RsfiPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "rsfi" do
    field :act, :non_neg_integer
    field :act_part, :non_neg_integer
    field :unknown, :integer, default: 0
    field :unknown2, :integer, default: 0
    field :ts, :non_neg_integer
    field :ts_max, :non_neg_integer
  end
end
