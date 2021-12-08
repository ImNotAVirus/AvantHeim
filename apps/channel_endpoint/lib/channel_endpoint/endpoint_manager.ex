defmodule ChannelEndpoint.EndpointManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias CachingService.Player.Session
  alias ChannelEndpoint.MapManager

  @manager_name __MODULE__

  ## Public API

  @spec start_link(keyword) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @manager_name)
  end

  @spec register_username(String.t()) :: :ok
  def register_username(username) do
    GenServer.call(@manager_name, {:register, username})
  end

  ## GenServer behaviour

  @impl true
  @spec init(any) :: {:ok, nil}
  def init(_) do
    Logger.info("EndpointManager started")
    # Map %{reference => username}
    {:ok, %{}}
  end

  @impl true
  def handle_call({:register, username}, {from, _tag}, state) do
    case get_key_by_value(state, username) do
      nil ->
        ref = Process.monitor(from)
        {:reply, :ok, Map.put(state, ref, username)}

      pid ->
        Logger.error("The PID #{inspect(pid)} is already registered for #{username}")
        {:reply, :error, state}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _object, reason}, state) do
    {username, updated_state} = Map.pop!(state, ref)
    {:ok, session} = CachingService.get_session_by_username(username)

    Logger.info("#{inspect(username)} is now disconnected (reason: #{reason}). Cleaning...")
    cleanup_session(session)
    CachingService.delete_session(username)

    {:noreply, updated_state}
  end

  ## Private functions

  defp get_key_by_value(map, value) do
    Enum.find_value(map, fn {key, val} -> if val == value, do: key end)
  end

  defp cleanup_session(%Session{state: :in_lobby}), do: :ok

  defp cleanup_session(%Session{state: :in_game, account_id: account_id} = session) do
    # Set the saving state to prevent player connection
    {:ok, %Session{}} =
      session
      |> Session.set_state(:saving)
      |> CachingService.update_session()

    # Clean Character cache
    {:ok, character} = CachingService.delete_character_by_account_id(account_id)

    # Clean map (out packet)
    MapManager.send_map_leave(character)

    # TODO: Save character in db
    # ...
  end
end
