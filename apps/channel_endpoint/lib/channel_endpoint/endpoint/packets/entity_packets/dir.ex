defmodule ChannelEndpoint.Endpoint.EntityPackets.Dir do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 2, direction_type: 2]

  alias __MODULE__
  alias DatabaseService.EntityEnums

  @enforce_keys [:entity_type, :entity_id, :direction]
  defstruct @enforce_keys

  @type t :: %Dir{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          direction: EntityEnums.direction_type_keys()
        }

  @impl true
  def serialize(%Dir{} = struct, _) do
    %Dir{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      direction: direction
    } = struct

    [
      "dir",
      entity_type(entity_type_atom, :value),
      entity_id,
      direction_type(direction, :value)
    ]
  end
end
