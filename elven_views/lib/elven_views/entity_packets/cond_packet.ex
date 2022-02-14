defmodule ElvenViews.EntityPackets.CondPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 2]

  alias __MODULE__
  alias ElvenEnums.EntityEnums

  @enforce_keys [:entity_type, :entity_id, :no_attack, :no_move, :speed]
  defstruct @enforce_keys

  @type t :: %CondPacket{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          no_attack: boolean,
          no_move: boolean,
          speed: non_neg_integer
        }

  @impl true
  def serialize(%CondPacket{} = struct, _) do
    %CondPacket{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      no_attack: no_attack,
      no_move: no_move,
      speed: speed
    } = struct

    ["cond", entity_type(entity_type_atom, :value), entity_id, no_attack, no_move, speed]
  end
end
