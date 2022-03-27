defmodule MapService.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  @type map_config :: map

  ## Public API

  @spec start_static_map(atom, non_neg_integer) :: {:ok, map_config()} | {:error, any}
  def start_static_map(manager, map_vnum) do
    GenServer.call(MapService.loader(manager), {:start_static_map, map_vnum})
  end
end
