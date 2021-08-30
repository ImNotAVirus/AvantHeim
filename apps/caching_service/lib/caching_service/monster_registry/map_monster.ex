defmodule CachingService.MonsterRegistry.MapMonster do
  @moduledoc """
  TODO: Documentation
  """

  alias __MODULE__

  @enforce_keys [:id, :vnum, :map_id, :map_x, :map_y]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          id: pos_integer,
          vnum: pos_integer,
          map_id: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer
        }

  @spec from_binary(binary, pos_integer) :: __MODULE__.t()
  def from_binary(bin, id) do
    [_, _, _, map_id, map_x, map_y, vnum, _, _] = String.split(bin, ",")

    %MapMonster{
      id: id,
      vnum: String.to_integer(vnum),
      map_id: String.to_integer(map_id),
      map_x: String.to_integer(map_x),
      map_y: String.to_integer(map_y)
    }
  end
end
