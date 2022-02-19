defmodule ElvenViews.PlayerPackets.CInfoPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.PlayerEnums,
    only: [family_rank: 1, gender: 1, hair_style: 1, hair_color: 1, character_class: 1]

  ## Packet definition

  defpacket "c_info" do
    field :character_id, :pos_integer
    field :name, :string
    field :group_id, :integer
    field :family_id, :integer
    field :family_rank, :enum, values: family_rank(:__enumerators__)
    # FIXME: String can be nil
    field :family_name, :string, as: :string
    field :name_color_id, :non_neg_integer
    field :gender, :enum, values: gender(:__enumerators__)
    field :hair_style, :enum, values: hair_style(:__enumerators__)
    field :hair_color, :enum, values: hair_color(:__enumerators__)
    field :class, :enum, values: character_class(:__enumerators__)
    field :reputation_icon_id, :integer
    field :compliment, :non_neg_integer
    field :morph, :non_neg_integer
    field :is_invisible, :boolean
    field :family_level, :non_neg_integer
    field :morph_upgrade, :non_neg_integer
    field :is_arena_winner, :boolean
  end
end
