defmodule CachingService.Map.Monster do
  @moduledoc """
  TODO: Documentation
  """

  alias __MODULE__

  import DatabaseService.EntityEnums, only: [direction_type: 1]

  @required_attributes [:id, :vnum, :map_id, :map_x, :map_y]

  @virtual_attributes %{
    name: nil,
    base_x: 0,
    base_y: 0,
    movement_radius: 5,
    level: 1,
    hp: 2_000,
    hp_max: 2_000,
    mp: 1_000,
    mp_max: 1_000,
    speed: 20,
    direction: :south,
    is_alive: true,
    is_sitting: true,
    is_invisible: false
  }

  use Memento.Table,
    type: :ordered_set,
    index: [:map_id],
    attributes: @required_attributes ++ Map.keys(@virtual_attributes)

  @type t :: %__MODULE__{
          id: pos_integer,
          vnum: pos_integer,
          map_id: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          name: String.t() | nil,
          base_x: non_neg_integer,
          base_y: non_neg_integer,
          movement_radius: non_neg_integer,
          level: pos_integer,
          hp: non_neg_integer,
          hp_max: non_neg_integer,
          mp: non_neg_integer,
          mp_max: non_neg_integer,
          speed: non_neg_integer,
          direction: atom,
          is_alive: boolean,
          is_sitting: boolean
        }

  ## Public API

  @spec from_binary(binary, pos_integer) :: __MODULE__.t()
  def from_binary(bin, id) do
    [_, _, _, map_id, map_x, map_y, vnum, _, _] = String.split(bin, ",")

    defaults = %{
      base_x: String.to_integer(map_x),
      base_y: String.to_integer(map_y),
      level: :rand.uniform(99),
      direction: Enum.random(direction_type(:__keys__)),
      is_sitting: Enum.random([true, false]),
      name: "Monster##{id}"
    }

    %Monster{
      id: id,
      vnum: String.to_integer(vnum),
      map_id: String.to_integer(map_id),
      map_x: String.to_integer(map_x),
      map_y: String.to_integer(map_y)
    }
    |> Map.merge(@virtual_attributes)
    |> Map.merge(defaults)
  end
end
