defmodule MapService.ConfigFile.MapNpc do
  @moduledoc false

  alias __MODULE__

  @enforce_keys [:map_npc_id, :vnum, :pos_x, :pos_y]
  defstruct @enforce_keys

  @type t :: %MapNpc{
          map_npc_id: pos_integer,
          vnum: non_neg_integer,
          pos_x: non_neg_integer,
          pos_y: non_neg_integer
        }

  ## Public API

  def new(config) do
    %MapNpc{
      map_npc_id: Map.fetch!(config, "map_npc_id"),
      vnum: Map.fetch!(config, "vnum"),
      pos_x: Map.fetch!(config, "pos_x"),
      pos_y: Map.fetch!(config, "pos_y")
    }
  end
end
