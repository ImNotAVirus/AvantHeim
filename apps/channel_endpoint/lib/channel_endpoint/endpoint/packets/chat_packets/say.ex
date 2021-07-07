defmodule ChannelEndpoint.Endpoint.ChatPackets.Say do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.ChatPackets.SayEnums, only: [color_type: 1]
  import DatabaseService.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :color, :message]
  defstruct @enforce_keys

  @type t :: %Say{
          entity_type: atom,
          entity_id: pos_integer,
          color: atom,
          message: String.t()
        }

  @impl true
  def serialize(%Say{} = struct, _) do
    %Say{
      entity_type: entity_type,
      entity_id: entity_id,
      color: color,
      message: message
    } = struct

    ["say", entity_type(entity_type), entity_id, color_type(color), message]
  end
end
