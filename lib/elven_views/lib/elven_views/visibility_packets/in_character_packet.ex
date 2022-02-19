defmodule ElvenViews.VisibilityPackets.InCharacterPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.MapEnums, only: [spawn_effect_type: 1]

  import ElvenEnums.EntityEnums,
    only: [
      entity_type: 1,
      element_type: 1,
      direction_type: 1
    ]

  import ElvenEnums.PlayerEnums,
    only: [
      authority: 1,
      gender: 1,
      faction: 1,
      hair_style: 1,
      hair_color: 1,
      character_class: 1
    ]

  alias ElvenViews.SubPackets.EquipmentSubPacket
  alias ElvenViews.SubPackets.FamilyIdRankSubPacket
  alias ElvenViews.SubPackets.ItemUpgradeRaritySubPacket

  ## Packet definition

  defpacket "in" do
    field :entity_type, :enum, values: entity_type(:__enumerators__), default: :character
    field :name, :string
    field :vnum, :string, default: "-"
    field :entity_id, :pos_integer
    field :map_x, :non_neg_integer
    field :map_y, :non_neg_integer
    field :direction, :enum, values: direction_type(:__enumerators__)
    field :authority, :enum, values: authority(:__enumerators__)
    field :gender, :enum, values: gender(:__enumerators__)
    field :hair_style, :enum, values: hair_style(:__enumerators__)
    field :hair_color, :enum, values: hair_color(:__enumerators__)
    field :class, :enum, values: character_class(:__enumerators__)
    field :equipments, EquipmentSubPacket
    field :hp_percent, :non_neg_integer
    field :mp_percent, :non_neg_integer
    field :is_sitting, :boolean
    field :group_id, :integer
    field :fairy_move_type_id, :non_neg_integer
    field :fairy_element, :enum, values: element_type(:__enumerators__)
    field :unknown1, :integer, default: 0
    field :fairy_morph, :non_neg_integer
    field :spawn_effect, :enum, values: spawn_effect_type(:__enumerators__), default: :summon
    field :morph, :non_neg_integer
    field :weapon_upgrade, ItemUpgradeRaritySubPacket
    field :armor_upgrade, ItemUpgradeRaritySubPacket
    field :family_id_rank, FamilyIdRankSubPacket
    field :family_name, :string, nullable: true
    field :reputation_icon_id, :integer
    field :is_invisible, :boolean
    field :morph_upgrade, :non_neg_integer
    field :faction, :enum, values: faction(:__enumerators__)
    field :morph_design, :non_neg_integer
    field :level, :non_neg_integer
    field :family_level, :non_neg_integer
    field :family_icons, :list, type: :boolean, joiner: "|"
    # FIXME: Probably not the is_arena_winner
    field :is_arena_winner, :boolean
    field :compliment, :non_neg_integer
    field :size, :pos_integer
    field :hero_level, :non_neg_integer
    field :title_id, :non_neg_integer
  end
end
