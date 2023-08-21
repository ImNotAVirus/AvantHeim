defmodule ElvenViews.EntityPackets.CondPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenViews.SerializablePacket

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  ## Packet definition

  defpacket "cond" do
    field :entity_type, :enum, values: entity_type(:__enumerators__)
    field :entity_id, :pos_integer
    field :no_attack, :boolean
    field :no_move, :boolean
    field :speed, :non_neg_integer
  end
end
