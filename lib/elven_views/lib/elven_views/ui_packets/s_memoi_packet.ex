defmodule ElvenViews.UIPackets.SMemoiPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenViews.UIPackets.BankEnums, only: [text_color: 1]

  alias ElvenI18n.PacketConstString

  ## Packet definition

  defpacket "s_memoi" do
    field :text_color, :enum, values: text_color(:__enumerators__)
    field :i18n_key, :string, apply: &i18n/1
    field :first_argument, :integer, default: 0
    field :second_argument, :integer, default: 0
  end

  ## Private function

  def i18n(key) do
    key
    |> PacketConstString.new!()
    |> Map.fetch!(:value)
  end
end
