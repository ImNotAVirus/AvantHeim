defmodule GameService.InitStaticMapSystem do
  @moduledoc """
  TODO: Documentation for GameService.InitStaticMapSystem

  TL;DR; Init all monster, npc, shop, etc... on a static map
  """

  use GameService.System, lock_components: []

  require Logger

  alias GameService.MonsterBundle

  ## System behaviour

  @impl true
  def run(%{partition: map_id}) do
    _ = load_map_tiles(map_id)
    _ = load_monsters(map_id)
  end

  ## Helpers

  defp max_concurrency(), do: Elixir.System.schedulers_online()
  defp priv_dir(), do: :code.priv_dir(:game_service)

  defp load_map_tiles(map_id) do
    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(map_cells_file(map_id))
    total_size = width * height
    <<_::bytes-size(total_size)>> = map_grid

    # TODO: Maybe create a module for the map grid put/get
    :ok = :persistent_term.put({:map_grid, map_id}, {width, height, map_grid})
  end

  defp map_cells_file(id) do
    Path.join(priv_dir(), "map_cells/#{id}")
  end

  defp load_monsters(map_id) do
    case YamlElixir.read_from_file(map_monsters_file(map_id)) do
      {:ok, data} ->
        data
        |> normalize_monsters(map_id)
        |> Task.async_stream(&spawn_monster/1, ordered: false, max_concurrency: max_concurrency())
        |> Stream.run()

      _ ->
        Logger.warn("no monster found for map #{map_id}")
    end
  end

  defp map_monsters_file(id) do
    Path.join(priv_dir(), "map_monster_placement/map_#{id}_monsters.yaml")
  end

  defp normalize_monsters(%{"map_id" => m, "monsters" => monsters}, map_id) when m == map_id do
    monsters
    |> Enum.map(&Map.take(&1, ~w(id vnum map_x map_y)))
    |> Enum.map(&Map.new(&1, fn {k, v} -> {String.to_atom(k), v} end))
    |> Enum.map(&Map.put(&1, :map_id, map_id))
  end

  defp spawn_monster(attrs) do
    # Spawn Monster entity into system
    specs = MonsterBundle.specs(attrs)
    {:ok, {_entity, _components}} = Command.spawn_entity(specs)

    # Here we can send an entity map enter event to the map partition
    # But it's not mendatory because the map just be created and there
    # is no player on it
  end
end
