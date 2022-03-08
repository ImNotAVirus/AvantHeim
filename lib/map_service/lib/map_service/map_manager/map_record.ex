defmodule MapService.MapManager.MapRecord do
  @moduledoc """
  TODO: Documentation
  """

  import Record, only: [defrecord: 2]

  defrecord :map_record, [:id, :vnum, :width, :height, :grid, :music_id]

  @type t ::
          record(:map_record,
            id: non_neg_integer,
            vnum: non_neg_integer,
            width: pos_integer,
            height: pos_integer,
            grid: binary,
            music_id: non_neg_integer
          )
end
