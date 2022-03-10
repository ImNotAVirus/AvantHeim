defmodule MapService.ConfigFile.MapConfig do
  @moduledoc false

  alias __MODULE__
  alias MapService.ConfigFile.MapPortal
  alias MapService.ConfigFile.MapNpc

  @enforce_keys [:id, :vnum, :width, :height, :grid, :music_id]
  @additional_keys [portals: [], npcs: []]
  defstruct @enforce_keys ++ @additional_keys

  @type t :: %MapConfig{
          id: non_neg_integer,
          vnum: non_neg_integer,
          width: non_neg_integer,
          height: non_neg_integer,
          grid: binary,
          music_id: non_neg_integer,
          portals: [MapPortal.t()],
          npcs: [MapNpc.t()]
        }

  ## Public API

  def new(config) do
    %{
      width: width,
      height: height,
      grid: grid
    } = fetch_grid_config(config)

    %MapConfig{
      id: Map.fetch!(config, "map_id"),
      vnum: Map.fetch!(config, "map_vnum"),
      width: width,
      height: height,
      grid: grid,
      music_id: Map.fetch!(config, "map_music_id"),
      portals: get_portals(config),
      npcs: get_npcs(config)
    }
  end

  ## Private functions

  defp fetch_grid_config(config) do
    grid_file = Map.fetch!(config, "grid_file")
    map_dir = Map.fetch!(config, "map_dir")

    filename = Path.join(map_dir, grid_file)

    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(filename)
    total_size = width * height
    <<_::bytes-size(total_size)>> = map_grid

    %{width: width, height: height, grid: map_grid}
  end

  defp get_portals(config) do
    portals = Map.get(config, "portals", [])
    Enum.map(portals, &MapPortal.new/1)
  end

  defp get_npcs(config) do
    npcs = Map.get(config, "npcs", [])
    Enum.map(npcs, &MapNpc.new/1)
  end
end
