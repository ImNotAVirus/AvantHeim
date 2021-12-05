defmodule ChannelEndpoint.MapManager.MapProcess do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer, restart: :temporary

  require Logger

  alias CachingService.Player.Character
  alias Core.Socket

  alias ChannelEndpoint.Endpoint.{
    EntityViews,
    MapViews,
    PlayerViews,
    VisibilityViews
  }

  @typep state :: %{id: pos_integer, character_ids: MapSet.t(pos_integer)}

  ## Public API

  @spec start_link(keyword) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    {map_id, gen_options} = Keyword.pop!(opts, :map_id)
    GenServer.start_link(__MODULE__, map_id, gen_options)
  end

  ## GenServer behaviour

  @impl true
  @spec init(pos_integer) :: {:ok, state(), :hibernate}
  def init(map_id) do
    Logger.debug("MapProcess##{map_id} started")
    state = %{id: map_id, character_ids: MapSet.new()}
    {:ok, state, :hibernate}
  end

  @impl true
  def handle_cast({:map_enter, %Character{} = entity}, state) do
    %Character{id: entity_id} = entity
    %{id: map_id} = state

    if MapSet.member?(state.character_ids, entity_id) do
      Logger.warn("Character##{entity_id} is already on map##{map_id}")
    end

    if MapSet.size(state.character_ids) == 0 do
      Logger.debug("First character on map. Wake up....")
      # TODO: Start or resume all monsters processes
    end

    map_enter(entity, map_id)
    {:noreply, Map.update!(state, :character_ids, &MapSet.put(&1, entity_id))}
  end

  def handle_cast({:map_leave, %Character{} = entity}, state) do
    %Character{id: entity_id} = entity
    %{id: map_id} = state

    if not MapSet.member?(state.character_ids, entity_id) do
      Logger.warn("Character##{entity_id} is not on map##{map_id}")
    end

    map_leave(entity, map_id)
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

  @spec map_enter(Character.t(), pos_integer) :: :ok
  defp map_enter(%Character{} = character, map_id) do
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
    {:ok, players} = CachingService.get_characters_by_map_id(map_id, [{:!==, :id, character.id}])
    Enum.each(players, &send_entity_visibility_packets(character, &1))

    # {:ok, monster} = CachingService.get_monsters_by_map_id(map_id)
    # Enum.each(monster, &send_entity_visibility_packets(character, &1))
  end

  @spec map_leave(Character.t(), pos_integer) :: any
  defp map_leave(%Character{} = character, map_id) do
    {:ok, _players} = CachingService.get_characters_by_map_id(map_id, [{:!==, :id, character.id}])
    raise "unimplemented map_leave"
  end

  @spec send_entity_visibility_packets(Character.t(), CachingService.entity()) ::
          :ok | {:error, atom}
  defp send_entity_visibility_packets(%Character{} = self, %Character{} = character) do
    Socket.send(self.socket, VisibilityViews.render(:in, character))
    Socket.send(character.socket, VisibilityViews.render(:in, self))
    Socket.send(self.socket, EntityViews.render(:c_mode, character))
    Socket.send(character.socket, EntityViews.render(:c_mode, self))
  end

  defp send_entity_visibility_packets(%Character{socket: socket}, entity) do
    Socket.send(socket, VisibilityViews.render(:in, entity))
  end
end
