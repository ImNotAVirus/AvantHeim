defmodule ChannelEndpoint.Endpoint.MapPackets.Rest do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :is_sitting]
  defstruct @enforce_keys

  @type t :: %Rest{
          entity_type: atom,
          entity_id: pos_integer,
          is_sitting: boolean
        }

  @impl true
  def serialize(%Rest{} = struct, _) do
    %Rest{
      entity_type: entity_type,
      entity_id: entity_id,
      is_sitting: is_sitting
    } = struct

    ["rest", entity_type(entity_type), entity_id, is_sitting]
  end
end
