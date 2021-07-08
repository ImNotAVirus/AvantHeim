defmodule ChannelEndpoint.Endpoint.MapPackets.CMap do
  @moduledoc """
  TODO: Documentation.

  A static a is a map wich is not an instance (raid, ts, ...)
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:map_vnum, :is_static_map]
  defstruct @enforce_keys

  @type t :: %CMap{
          map_vnum: pos_integer,
          is_static_map: boolean
        }

  @impl true
  def serialize(%CMap{} = struct, _) do
    %CMap{
      map_vnum: map_vnum,
      is_static_map: is_static_map
    } = struct

    ["c_map", 0, map_vnum, is_static_map]
  end
end
