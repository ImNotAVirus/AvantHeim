defmodule ElvenViews.LobbyPackets.ClistPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.PlayerEnums

  alias ElvenViews.SubPackets.EquipmentSubPacket

  ## Packet definition

  defpacket "clist" do
    field :slot, :non_neg_integer
    field :name, :string
    field :unknown1, :integer, default: 0
    field :gender, :enum, values: gender(:__enumerators__)
    field :hair_style, :enum, values: hair_style(:__enumerators__)
    field :hair_color, :enum, values: hair_color(:__enumerators__)
    field :unknown2, :integer, default: 0
    field :class, :enum, values: character_class(:__enumerators__)
    field :level, :non_neg_integer
    field :hero_level, :non_neg_integer
    field :equipments, EquipmentSubPacket
    field :job_level, :non_neg_integer
    field :quest_completion, :non_neg_integer, default: 1
    field :quest_part, :non_neg_integer, default: 1
    field :pets, :list, type: :integer, joiner: "."
    field :design, :non_neg_integer
  end
end
