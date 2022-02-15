defmodule ElvenViews.EntityPackets.DirPacket do
  @moduledoc """
  Player direction
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 2, direction_type: 2]

  alias __MODULE__
  alias ElvenEnums.EntityEnums

  @enforce_keys [:entity_type, :entity_id, :direction_type]
  defstruct @enforce_keys

  @type t :: %DirPacket{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          direction_type: EntityEnums.direction_type_keys()
        }

  @impl true
  def serialize(%DirPacket{} = struct, _) do
    %DirPacket{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      direction_type: direction_type_atom
    } = struct

    [
      "dir",
      entity_type(entity_type_atom, :value),
      entity_id,
      direction_type(direction_type_atom, :value)
    ]
  end
end
