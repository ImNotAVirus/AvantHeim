defmodule CachingService.Position do
  @enforce_keys [:map_id, :map_vnum, :map_x, :map_y, :is_instance]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          map_id: pos_integer | reference,
          map_vnum: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          map_y: boolean
        }
end
