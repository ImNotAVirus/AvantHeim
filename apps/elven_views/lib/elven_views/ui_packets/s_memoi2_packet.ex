defmodule ElvenViews.UIPackets.SMemoi2Packet do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.UIPackets.BankEnums, only: [text_color: 1]

  alias ElvenViews.SubPackets.I18nSubPacket

  ## Packet definition

  defpacket "s_memoi2" do
    field :text_color, :enum, default: :white, values: text_color(:__enumerators__)
    field :i18n_packet, I18nSubPacket
  end
end
