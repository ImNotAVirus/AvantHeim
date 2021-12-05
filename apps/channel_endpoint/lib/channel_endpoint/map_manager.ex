defmodule ChannelEndpoint.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.MapRegistry.MapRecord

  alias CachingService.MapRegistry
  alias CachingService.MapRegistry.MapRecord
  alias CachingService.Player.Character
  alias CachingService.Position
  alias ChannelEndpoint.MapManager.MapProcess

  @map_supervisor ChannelEndpoint.MapProcesses
  @map_registry ChannelEndpoint.MapRegistry
  @map_manager __MODULE__
  @maps_path "#{:code.priv_dir(:caching_service)}/client_files/maps"

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @map_manager)
  end

  @spec change_map(Character.t(), Position.t()) :: :ok
  def change_map(%Character{} = character, %Position{} = pos) do
    send_map_leave(character)

    {:ok, updated_character} =
      character
      |> Character.set_position(pos)
      |> CachingService.write_character()

    send_map_enter(updated_character)
  end

  @doc """
  Send packets for a player enter on a map.

  DOESN'T modify the cache
  """
  @spec send_map_enter(CachingService.entity()) :: :ok
  def send_map_enter(%Character{} = character) do
    %Position{map_id: map_id} = Character.get_position(character)
    GenServer.cast(map_process(map_id), {:map_enter, character})
  end

  @doc """
  Send packets for a player leaving a map.

  DOESN'T modify the cache
  """
  @spec send_map_leave(CachingService.entity()) :: :ok
  def send_map_leave(%Character{} = character) do
    %Position{map_id: map_id} = Character.get_position(character)
    GenServer.cast(map_process(map_id), {:map_leave, character})
  end

  ### change_character_map
  ### broadcast_to_map
  ### broadcast_to_channel

  ### Cache map in ETS then spawn map process
  ### Monitor map process and if it die, remove entry from ETS

  ## GenServer behaviour

  @impl true
  @spec init(any) :: {:ok, nil, {:continue, :init_maps}}
  def init(_) do
    Logger.info("MapManager starting...")
    {:ok, nil, {:continue, :init_maps}}
  end

  @impl true
  @spec handle_continue(:init_maps, nil) :: {:noreply, state :: map}
  def handle_continue(:init_maps, nil) do
    map_files = Path.wildcard("#{@maps_path}/?")

    state =
      map_files
      |> Stream.map(&parse_map_file/1)
      |> Stream.map(&persist_map/1)
      |> Stream.map(&map_record(&1, :id))
      |> Enum.map(&spawn_map_process/1)
      |> Map.new()

    Logger.info("MapManager is now monitoring #{map_size(state)} maps")

    {:noreply, state}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, reason}, state) when is_pid(pid) do
    {map_id, updated_state} = Map.pop!(state, pid)
    Logger.warn("MapProcess##{map_id} halted with reason #{inspect(reason)}. Cleaning...")

    # TODO: Remove all players on map
    # TODO: Remove all monsters on map
    # .....

    # Spawn a new monitored MapProcess
    {map_pid, map_id} = spawn_map_process(map_id)

    {:noreply, Map.put(updated_state, map_pid, map_id)}
  end

  ## Private functions

  @spec parse_map_file(String.t()) :: MapRecord.t()
  defp parse_map_file(filename) do
    Logger.debug("Parsing map file: #{filename}")

    id = filename |> Path.basename() |> String.to_integer(10)

    <<width::16-little, height::16-little, map_bin::binary>> = File.read!(filename)
    total_size = width * height
    <<_::bytes-size(total_size)>> = map_bin

    Logger.debug("Map parsed: id=#{id} size=#{width}x#{height}")

    map_record(
      id: id,
      width: width,
      height: height,
      bin: map_bin
    )
  end

  @spec persist_map(MapRecord.t()) :: MapRecord.t()
  defp persist_map(record) do
    {:ok, ^record} = MapRegistry.insert_new(record)
    record
  end

  @spec spawn_map_process(pos_integer) :: any
  defp spawn_map_process(map_id) do
    spec = {MapProcess, map_id: map_id, name: map_process(map_id)}
    {:ok, map_pid} = DynamicSupervisor.start_child(@map_supervisor, spec)
    _map_mon_ref = Process.monitor(map_pid)
    {map_pid, map_id}
  end

  @spec map_process(pos_integer) :: {:via, Registry, {module, pos_integer}}
  defp map_process(map_id) do
    {:via, Registry, {@map_registry, map_id}}
  end
end
