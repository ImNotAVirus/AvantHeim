defmodule ChannelService.Endpoint.MapPackets.At do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.EntityEnums, only: [direction_type: 1]

  alias __MODULE__

  @enforce_keys [:character_id, :map_vnum, :map_x, :map_y, :direction, :map_music]
  defstruct @enforce_keys

  @type t :: %At{
          character_id: pos_integer,
          map_vnum: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          direction: atom,
          map_music: non_neg_integer
        }

  @impl true
  def serialize(%At{} = struct, _) do
    %At{
      character_id: character_id,
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y,
      direction: direction,
      map_music: map_music
    } = struct

    [
      "at",
      character_id,
      map_vnum,
      map_x,
      map_y,
      direction_type(direction),
      0,
      map_music,
      2,
      -1
    ]
  end
end
