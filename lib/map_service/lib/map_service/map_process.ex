defmodule MapService.MapProcess do
  @moduledoc false

  use GenServer

  require Logger

  alias ElvenCore.Socket
  alias ElvenCaching.CharacterRegistry
  alias ElvenCaching.Entity.Character
  alias ElvenCaching.Entity.EntityPosition
  alias MapService.ConfigFile.MapConfig

  alias ElvenViews.{
    EntityViews,
    MapViews,
    PlayerViews,
    VisibilityViews
  }

  # @typep state :: %{config: MapConfig.t(), character_ids: MapSet.t(pos_integer)}

  ## Public API

  @spec start_link({MapConfig.t(), GenServer.options()}) :: GenServer.on_start()
  def start_link({config, process_opts}) do
    GenServer.start_link(__MODULE__, config, process_opts)
  end

  ## GenServer behaviour

  @impl true
  def init(config) do
    Logger.debug("MapProcess##{config.id} started")
    {:ok, %{config: config, character_ids: MapSet.new()}, :hibernate}
  end

  @impl true
  def handle_cast({:map_enter, %Character{} = entity}, %{config: config} = state) do
    %Character{id: entity_id} = entity
    %{id: map_id} = config

    if MapSet.member?(state.character_ids, entity_id) do
      Logger.warn("Character##{entity_id} is already on map##{inspect(map_id)}")
    end

    if MapSet.size(state.character_ids) == 0 do
      Logger.debug("First character on map. Wake up....")
      # TODO: Start or resume all monsters processes
    end

    send_map_enter_packets(entity, map_id)
    {:noreply, Map.update!(state, :character_ids, &MapSet.put(&1, entity_id))}
  end

  def handle_cast({:map_leave, %Character{} = entity}, %{config: config} = state) do
    %Character{id: entity_id} = entity
    %{id: map_id} = config

    if not MapSet.member?(state.character_ids, entity_id) do
      Logger.warn("Character##{entity_id} is not on map##{inspect(map_id)}")
    end

    send_map_leave_packets(entity, map_id)
    updated_state = Map.update!(state, :character_ids, &MapSet.delete(&1, entity_id))

    case MapSet.size(updated_state.character_ids) do
      0 ->
        Logger.debug("No more character on map. Hibernating....")
        # TODO: Pause all monsters processes
        {:noreply, updated_state, :hibernate}

      _ ->
        {:noreply, updated_state}
    end
  end

  ## Private functions

  @spec send_map_enter_packets(Character.t(), EntityPosition.map_id()) :: :ok
  defp send_map_enter_packets(%Character{} = character, map_id) do
    ## Self packets
    Socket.send(character.socket, PlayerViews.render(:c_info, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, PlayerViews.render(:lev, character))
    Socket.send(character.socket, PlayerViews.render(:stat, character))
    Socket.send(character.socket, MapViews.render(:at, character))
    Socket.send(character.socket, MapViews.render(:c_map, character))
    # TODO: Socket.send(character.socket, PlayerViews.render(:sc, character))
    Socket.send(character.socket, EntityViews.render(:char_sc, character))
    Socket.send(character.socket, EntityViews.render(:cond, character))

    ## Visibility packets
    {:ok, players} = CharacterRegistry.get_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_entity_enter_packets(character, &1))

    # {:ok, monster} = MonsterRegistry.get_by_map_id(map_id)
    # Enum.each(monster, &send_entity_enter_packets(character, &1))
  end

  @spec send_entity_enter_packets(Character.t(), ElvenCaching.entity()) :: :ok | {:error, atom}
  defp send_entity_enter_packets(%Character{} = self, %Character{} = character) do
    Socket.send(self.socket, VisibilityViews.render(:in, character))
    Socket.send(character.socket, VisibilityViews.render(:in, self))
    Socket.send(self.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, self))
  end

  defp send_entity_enter_packets(%Character{socket: socket}, entity) do
    Socket.send(socket, VisibilityViews.render(:in, entity))
  end

  @spec send_map_leave_packets(Character.t(), EntityPosition.map_id()) :: :ok
  defp send_map_leave_packets(%Character{} = character, map_id) do
    ## Self packets
    Socket.send(character.socket, MapViews.render(:mapout, character))

    ## Visibility packets
    {:ok, players} = CharacterRegistry.get_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_entity_leave_packets(&1, character))
  end

  @spec send_entity_leave_packets(Character.t(), ElvenCaching.entity()) :: :ok | {:error, atom}
  defp send_entity_leave_packets(%Character{} = character, entity) do
    Socket.send(character.socket, VisibilityViews.render(:out, entity))
  end
end
