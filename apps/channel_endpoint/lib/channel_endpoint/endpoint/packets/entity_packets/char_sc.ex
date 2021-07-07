defmodule ChannelEndpoint.Endpoint.EntityPackets.CharSc do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :size]
  defstruct @enforce_keys

  @type t :: %CharSc{
          entity_type: atom,
          entity_id: pos_integer,
          size: pos_integer
        }

  @impl true
  def serialize(%CharSc{} = struct, _) do
    %CharSc{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      size: size
    } = struct

    ["char_sc", entity_type(entity_type_atom), entity_id, size]
  end
end
