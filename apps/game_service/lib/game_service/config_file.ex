defmodule GameService.ConfigFile do
  @moduledoc """
  TODO: Documentation for GameService.ConfigFile

  Note: all functions can raise on invalid files
  """

  @type map_id :: non_neg_integer()

  ## Public API

  def static_map_ids() do
    file = Path.join(priv_dir(), "maps/official_maps.yaml")

    file
    |> YamlElixir.read_from_file!()
    |> Enum.filter(&("IS_BASE_MAP" in (&1["flags"] || [])))
    |> Enum.map(& &1["map_id"])
  end

  @spec map_grid(map_id()) :: {width, height, binary}
        when width: non_neg_integer(), height: non_neg_integer()
  def map_grid(map_id) do
    file = Path.join(priv_dir(), "map_cells/#{map_id}")

    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(file)
    total_size = width * height

    # Just check that the file is valid
    <<_::bytes-size(total_size)>> = map_grid

    {width, height, map_grid}
  end

  @spec map_monsters(map_id()) :: [map()]
  def map_monsters(map_id) do
    file = Path.join(priv_dir(), "map_monster_placement/map_#{map_id}_monsters.yaml")

    case YamlElixir.read_from_file(file) do
      {:ok, %{"monsters" => monsters}} ->
        monsters
        |> Enum.map(&Map.take(&1, ~w(id vnum map_x map_y)))
        |> Enum.map(&Map.new(&1, fn {k, v} -> {String.to_atom(k), v} end))
        |> Enum.map(&Map.put(&1, :map_id, map_id))

      _ ->
        []
    end
  end

  ## Helpers

  defp priv_dir(), do: :code.priv_dir(:game_service)
end
