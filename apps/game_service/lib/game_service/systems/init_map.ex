defmodule GameService.InitMapSystem do
  @moduledoc """
  TODO: Documentation for GameService.InitMapSystem

  TL;DR; Init all monster, npc, shop, etc... on a map
  """

  use GameService.System, lock_components: []

  require Logger

  alias GameService.GameConfig
  alias GameService.MonsterBundle

  @timeout :timer.seconds(30)

  ## System behaviour

  @impl true
  def run(%{partition: map_id}) do
    Task.await_many(
      [
        Task.async(fn -> load_monsters(map_id) end)
      ],
      @timeout
    )
  end

  ## Helpers

  defp load_monsters(map_id) do
    map_id
    |> GameConfig.map_monsters()
    |> Enum.each(&spawn_monster/1)
  end

  defp spawn_monster(attrs) do
    # Spawn Monster entity into system
    specs = MonsterBundle.specs(attrs)
    {:ok, {_entity, _components}} = Command.spawn_entity(specs)

    # Here we can send an entity map enter event to the map partition
    # But it's not mendatory because the map just be created and there
    # is no player on it
  catch
    kind, payload ->
      error = Exception.format(kind, payload, __STACKTRACE__)
      Logger.error("invalid monster attrs: #{inspect(attrs)}\n#{error}")
  end
end
