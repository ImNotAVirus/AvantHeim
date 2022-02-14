defmodule ElvenViews.EntityPackets.CharScPacket do
  @moduledoc """
  TODO: Documentation.
  Character model
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 2]

  alias __MODULE__
  alias ElvenEnums.EntityEnums

  @enforce_keys [
    :entity_type,
    :entity_id,
    :size
  ]
  defstruct @enforce_keys

  @type t :: %CharScPacket{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          size: pos_integer
        }

  @impl true
  def serialize(%CharScPacket{} = struct, _) do
    %CharScPacket{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      size: size
    } = struct

    List.flatten([
      ["char_sc", entity_type(entity_type_atom, :value), entity_id, size]
    ])
  end
end
