defmodule MapService.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenCaching.CharacterRegistry
  alias ElvenCaching.Entity.EntityPosition
  alias ElvenCaching.Entity.Character
  alias ElvenCaching.MapEntity

  @type map_config :: map

  ## Public API

  @spec start_static_map(non_neg_integer) :: {:ok, map_config()} | {:error, any}
  def start_static_map(map_vnum) do
    GenServer.call(MapService.loader(), {:start_static_map, map_vnum})
  end

  @doc """
  Set a new position for a character and send packets (map in and out)
  """
  @spec set_map(Character.t(), EntityPosition.t()) :: :ok
  def set_map(%Character{} = character, %EntityPosition{} = pos) do
    if character.map_id do
      # send_map_leave(character)
    end

    {:ok, _updated_character} =
      character
      |> MapEntity.position(pos)
      |> CharacterRegistry.write()

    # send_map_enter(updated_character)
  end
end
