defmodule ElvenViews.EntityPackets.CModePacket do
  @moduledoc """
  TODO: Documentation.
  Character model
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  ## Packet definition

  defpacket "c_mode" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :morph, :non_neg_integer
    field :morph_upgrade, :non_neg_integer
    field :morph_design, :non_neg_integer
    field :is_arena_winner, :boolean
    field :size, :pos_integer
    field :item_morph, :non_neg_integer
  end
end
