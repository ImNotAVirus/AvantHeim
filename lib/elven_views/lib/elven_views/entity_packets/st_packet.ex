defmodule ElvenViews.EntityPackets.StPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  ## Packet definition

  defpacket "st" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :level, :non_neg_integer
    field :hero_level, :non_neg_integer
    field :hp_percent, :non_neg_integer
    field :mp_percent, :non_neg_integer
    field :hp, :non_neg_integer
    field :mp, :non_neg_integer
    field :buffs, :list, type: :pos_integer, default: :drop, joiner: " "
  end
end
