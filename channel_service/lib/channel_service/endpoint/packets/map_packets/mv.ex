defmodule ChannelService.Endpoint.MapPackets.Mv do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [entity_type: 1]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :map_x, :map_y, :speed]
  defstruct @enforce_keys

  @type t :: %Mv{
          entity_type: atom,
          entity_id: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          speed: non_neg_integer
        }

  @impl true
  def serialize(%Mv{} = struct, _) do
    %Mv{
      entity_type: entity_type,
      entity_id: entity_id,
      map_x: map_x,
      map_y: map_y,
      speed: speed
    } = struct

    ["mv", entity_type(entity_type), entity_id, map_x, map_y, speed]
  end
end
