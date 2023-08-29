defmodule ElvenPackets.Server.EntityPackets do
  @moduledoc """
  TODO: ElvenPackets.Server.EntityPackets
  """

  use ElvenGard.Network.PacketSerializer

  import ElvenEnums.EntityEnums, only: [entity_type: 1, morph: 1, wings_design: 1, direction_type: 1]

  alias ElvenPackets.Types.{NsEnum, NsInteger, NsString, NsBoolean, NsList}

  @serializable true
  defpacket "c_mode", as: CMode do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :morph, NsEnum, values: morph(:__enumerators__)
    field :morph_upgrade, NsInteger
    field :wings_design, NsEnum, values: wings_design(:__enumerators__)
    field :is_arena_winner, NsBoolean
    field :size, NsInteger
    field :item_morph, NsInteger
  end

  @serializable true
  defpacket "char_sc", as: CharSc do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :size, NsInteger
  end

  @serializable true
  defpacket "cond", as: Cond do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :no_attack, NsBoolean
    field :no_move, NsBoolean
    field :speed, NsInteger
  end

  @serializable true
  defpacket "dir", as: Dir do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :direction, NsEnum, values: direction_type(:__enumerators__)
  end

  @serializable true
  defpacket "eff", as: Eff do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :value, NsInteger
  end

  @serializable true
  defpacket "st", as: St do
    field :entity_type, NsEnum, values: entity_type(:__enumerators__)
    field :entity_id, NsInteger
    field :level, NsInteger
    field :hero_level, NsInteger
    field :hp_percent, NsInteger
    field :mp_percent, NsInteger
    field :hp, NsInteger
    field :mp, NsInteger
    field :buffs, NsList, type: NsInteger, default: :drop, joiner: " "
  end
end
