defmodule ElvenPackets.Server.VisibilityPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.VisibilityPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenData.Enums.EntityEnums,
    only: [
      entity_type: 1,
      element_type: 1,
      direction_type: 1
    ]

  import ElvenData.Enums.MapEnums, only: [spawn_effect_type: 1]

  import ElvenData.Enums.PlayerEnums,
    only: [
      authority: 1,
      character_class: 1,
      faction: 1,
      gender: 1,
      hair_style: 1,
      hair_color: 1,
      morph: 1,
      reputation_icon: 1,
      wings_design: 1
    ]

  alias ElvenPackets.SubPackets.Equipment
  alias ElvenPackets.SubPackets.Player.Family
  alias ElvenPackets.SubPackets.Item.UpgradeRarity

  alias ElvenPackets.Types.{NsBoolean, NsEnum, NsInteger, NsList, NsString}

  ## Visibility packets

  @serializable true
  defpacket "in", as: InPlayer do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__), default: :player
    field :name, NsString
    field :vnum, NsString, default: "-"
    field :entity_id, NsInteger
    field :map_x, NsInteger
    field :map_y, NsInteger
    field :direction, NsEnum, values: direction_type(:__enumerators__)
    field :authority, NsEnum, values: authority(:__enumerators__)
    field :gender, NsEnum, values: gender(:__enumerators__)
    field :hair_style, NsEnum, values: hair_style(:__enumerators__)
    field :hair_color, NsEnum, values: hair_color(:__enumerators__)
    field :class, NsEnum, values: character_class(:__enumerators__)
    field :equipments, Equipment
    field :hp_percent, NsInteger
    field :mp_percent, NsInteger
    field :is_sitting, NsBoolean
    field :group_id, NsInteger
    field :fairy_move_type_id, NsInteger
    field :fairy_element, NsEnum, values: element_type(:__enumerators__)
    field :unknown1, NsInteger, default: 0
    field :fairy_morph, NsInteger
    field :spawn_effect, NsEnum, values: spawn_effect_type(:__enumerators__), default: :summon
    field :morph, NsEnum, default: :default, values: morph(:__enumerators__)
    field :weapon_upgrade, UpgradeRarity
    field :armor_upgrade, UpgradeRarity
    field :family, Family
    field :reputation_icon, NsEnum, values: reputation_icon(:__enumerators__)
    field :is_invisible, NsBoolean
    field :morph_upgrade, NsInteger
    field :faction, NsEnum, values: faction(:__enumerators__)
    field :wings_design, NsEnum, default: :default, values: wings_design(:__enumerators__)
    field :level, NsInteger
    field :family_level, NsInteger
    field :family_icons, NsList, type: NsBoolean, joiner: "|"
    # FIXME: Probably not the is_arena_winner
    field :is_arena_winner, NsBoolean
    field :compliment, NsInteger
    field :size, NsInteger
    field :hero_level, NsInteger
    field :title_id, NsInteger
  end

  @serializable true
  defpacket "in", as: InNpcOrMonster do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :vnum, NsInteger
    field :entity_id, NsInteger
    field :map_x, NsInteger
    field :map_y, NsInteger
    field :direction, NsEnum, values: direction_type(:__enumerators__)
    field :hp_percent, NsInteger
    field :mp_percent, NsInteger
    field :dialog, NsInteger, default: -1
    field :unknown1, NsInteger, default: 0
    field :unknown2, NsInteger, default: 0
    field :unknown3, NsInteger, default: -1
    field :spawn_effect, NsEnum, values: spawn_effect_type(:__enumerators__), default: :no_effect
    field :is_sitting, NsBoolean
    field :unknown4, NsInteger, default: -1
    field :name, NsString, default: "-"
    field :unknown5, NsInteger, default: 0
    field :unknown6, NsInteger, default: -1
    field :unknown7, NsInteger, default: 0
    field :unknown8, NsInteger, default: 0
    field :unknown9, NsInteger, default: 0
    field :unknown10, NsInteger, default: 0
    field :unknown11, NsInteger, default: 0
    field :unknown12, NsInteger, default: 0
    field :unknown13, NsInteger, default: 0
    field :is_invisible, NsBoolean
    field :unknown14, NsInteger, default: 0
    field :unknown15, NsInteger, default: 0
    field :unknown16, NsInteger, default: 0
    field :stars, NsInteger, default: 0
  end

  @serializable true
  defpacket "out", as: Out do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
  end
end
