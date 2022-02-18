defmodule ChannelService.Endpoint.VisibilityPackets.Out do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id]
  defstruct @enforce_keys

  @type t :: %Out{entity_type: atom, entity_id: pos_integer}

  @impl true
  def serialize(%Out{} = struct, _) do
    %Out{entity_type: entity_type_atom, entity_id: entity_id} = struct
    ["out", entity_type(entity_type_atom), entity_id]
  end
end
