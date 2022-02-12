defmodule ElvenViews.ChatPackets.SayPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ElvenViews.ChatPackets.SayEnums, only: [color_type: 2]
  import DatabaseService.EntityEnums, only: [entity_type: 2]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :color, :message]
  defstruct @enforce_keys

  @type t :: %SayPacket{
          entity_type: atom,
          entity_id: pos_integer,
          color: atom,
          message: String.t()
        }

  @impl true
  def serialize(%SayPacket{} = struct, _) do
    %SayPacket{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      color: color,
      message: message
    } = struct

    ["say", entity_type(entity_type_atom, :value), entity_id, color_type(color, :value), message]
  end
end
