defmodule ElvenViews.UIPackets.GbPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.UIPackets.BankEnums, only: [action_type: 1]

  ## Packet definition

  defpacket "gb" do
    field :action_type, :enum, values: action_type(:__enumerators__)
    field :bank_gold, :non_neg_integer
    field :gold, :non_neg_integer
    field :bank_rank, :pos_integer
    field :bank_tax, :non_neg_integer
  end
end
