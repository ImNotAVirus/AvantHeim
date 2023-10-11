defmodule GameService.InitStaticMapSystem do
  @moduledoc """
  TODO: Documentation for GameService.InitStaticMapSystem

  TL;DR; Init all monster, npc, shop, etc... on a static map
  """

  use GameService.System, lock_components: []

  require Logger

  alias GameService.ConfigFile
  alias GameService.MonsterBundle

  ## System behaviour

  @impl true
  def run(%{partition: map_id}) do
    _ = load_map_tiles(map_id)
    _ = load_monsters(map_id)
  end

  ## Helpers

  defp max_concurrency(), do: Elixir.System.schedulers_online()

  defp load_map_tiles(map_id) do
    # TODO: Maybe create a module for the map grid put/get
    :ok = :persistent_term.put({:map_grid, map_id}, ConfigFile.map_grid(map_id))
  end

  defp load_monsters(map_id) do
    case ConfigFile.map_monsters(map_id) do
      [] ->
        Logger.warn("no monster found for map #{map_id}")

      monsters ->
        monsters
        |> Task.async_stream(&spawn_monster/1, ordered: false, max_concurrency: max_concurrency())
        |> Stream.run()
    end
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
