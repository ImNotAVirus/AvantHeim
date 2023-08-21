defmodule ElvenViews.PlayerPackets.StatPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  ## Packet definition

  defpacket "stat" do
    field :hp, :non_neg_integer
    field :hp_max, :non_neg_integer
    field :mp, :non_neg_integer
    field :mp_max, :non_neg_integer
    field :unknown, :integer, default: 0
    field :option, :integer
  end
end
