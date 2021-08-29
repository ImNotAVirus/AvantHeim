defmodule ChannelEndpoint.Endpoint.EntityPackets.Dir do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 2]

  alias __MODULE__

  @enforce_keys [:direction, :entity_type, :entity_id]
  defstruct @enforce_keys

  @type t :: %Dir{
    direction: non_neg_integer,
    entity_type: atom,
    entity_id: pos_integer
  }

  @impl true
  def serialize(%Dir{} = struct, _) do
    %Dir{
      direction: direction,
      entity_type: entity_type_atom,
      entity_id: entity_id
    } = struct

    ["dir", direction, entity_type(entity_type_atom, :value), entity_id]
  end
end
