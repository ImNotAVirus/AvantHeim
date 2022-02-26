defmodule ElvenViews.UIPackets.SMemoi2Packet do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.UIPackets.BankEnums, only: [text_color: 1]

  alias ElvenI18n.PacketConstString

  ## Packet definition

  defpacket "s_memoi2" do
    field :text_color, :enum, values: text_color(:__enumerators__)
    field :i18n_key, :string, apply: &i18n/1
    field :argument_count, :integer, default: 3
    field :bank_gold, :non_neg_integer, apply: &ElvenCore.format_number/1
    field :gold, :non_neg_integer, apply: &ElvenCore.format_number/1
    field :unknown, :integer, default: 0
  end

  ## Private function

  def i18n(key) do
    key
    |> PacketConstString.new!()
    |> Map.fetch!(:value)
  end
end
