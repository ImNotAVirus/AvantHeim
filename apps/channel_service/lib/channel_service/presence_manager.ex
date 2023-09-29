defmodule ChannelService.PresenceManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias ElvenCaching.Account.Session
  alias ElvenCaching.SessionRegistry

  alias GameService.Events.PlayerDisconnected

  @manager_name __MODULE__

  ## Public API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @manager_name)
  end

  @spec register_username(String.t()) :: :ok
  def register_username(username) do
    GenServer.call(@manager_name, {:register, username})
  end

  ## GenServer behaviour

  @impl true
  @spec init(any) :: {:ok, %{}}
  def init(_) do
    Logger.info("PresenceManager started")
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
    {:ok, session} = SessionRegistry.get(username)

    Logger.info("#{inspect(username)} is now disconnected (reason: #{inspect(reason)})")
    Logger.info("Cleaning session...")

    cleanup_session(session)
    SessionRegistry.delete(username)

    {:noreply, updated_state}
  end

  ## Private functions

  defp get_key_by_value(map, value) do
    Enum.find_value(map, fn {key, val} -> if val == value, do: key end)
  end

  defp cleanup_session(%Session{state: :in_lobby}), do: :ok

  defp cleanup_session(%Session{state: :in_game, account_id: account_id}) do
    # Remove Entity from ECS and notify all Frontends
    {:ok, _events} =
      ElvenGard.ECS.push(
        %PlayerDisconnected{account_id: account_id},
        # Here we don't know the map_ref so we will send the event to a special partition
        partition: :system
      )

    # TODO: Save character in db
    # ...
  end
end
