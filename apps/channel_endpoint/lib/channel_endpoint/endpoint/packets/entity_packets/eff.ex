defmodule ChannelEndpoint.Endpoint.EntityPackets.Eff do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :value]
  defstruct @enforce_keys

  @type t :: %Eff{
          entity_type: atom,
          entity_id: pos_integer,
          value: pos_integer
        }

  @impl true
  def serialize(%Eff{} = struct, _) do
    %Eff{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      value: value
    } = struct

    ["eff", entity_type(entity_type_atom), entity_id, value]
  end
end
