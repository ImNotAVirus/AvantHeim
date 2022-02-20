defmodule ElvenViews.PlayerPackets.TitPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.PlayerEnums, only: [character_class: 1]

  alias ElvenI18n.PacketConstString

  ## Packet definition

  defpacket "tit" do
    field :class, :enum, values: character_class(:__enumerators__), apply: &i18n/1
    field :name, :string
  end

  ## Private function

  def i18n(enum) do
    enum
    |> character_class()
    |> enum_to_i18n_key()
    |> PacketConstString.new!()
    |> Map.fetch!(:value)
  end

  # i18n value: 35
  defp enum_to_i18n_key(:adventurer), do: "Adventurer"
  # i18n value: 36
  defp enum_to_i18n_key(:swordman), do: "Swordsman"
  # i18n value: 37
  defp enum_to_i18n_key(:archer), do: "Archer"
  # i18n value: 38
  defp enum_to_i18n_key(:magician), do: "Mage"
  # i18n value: 39
  defp enum_to_i18n_key(:martial_artist), do: "MartialArtist"
end
