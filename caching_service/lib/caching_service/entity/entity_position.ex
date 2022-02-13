defmodule CachingService.Entity.EntityPosition do
  @moduledoc false

  @enforce_keys [:map_id, :map_vnum, :map_x, :map_y, :is_instance]
  defstruct @enforce_keys

  alias __MODULE__

  # pos_integer: static map - reference: instance
  @type map_id :: pos_integer | reference
  @type t :: %EntityPosition{
          map_id: map_id(),
          map_vnum: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          is_instance: boolean
        }

  @doc """
  Create new position structure for a static map
  """
  @spec new(map_id(), non_neg_integer, non_neg_integer) :: EntityPosition.t()
  def new(map_id, map_x, map_y) do
    %EntityPosition{
      map_id: map_id,
      map_vnum: map_id,
      map_x: map_x,
      map_y: map_y,
      is_instance: false
    }
  end

  @doc """
  Create new position structure for a static or instance map
  """
  @spec new(map_id(), pos_integer, non_neg_integer, non_neg_integer) :: EntityPosition.t()
  def new(map_id, map_vnum, map_x, map_y) do
    %EntityPosition{
      map_id: map_id,
      map_vnum: map_vnum,
      map_x: map_x,
      map_y: map_y,
      is_instance: is_reference(map_vnum)
    }
  end
end
