defmodule ChannelService.PresenceManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias GameService.Events.PlayerDisconnected

  @manager_name __MODULE__

  ## Public API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @manager_name)
  end

  @spec register(String.t(), non_neg_integer()) :: :ok | {:error, :already_registered}
  def register(username, account_id) do
    GenServer.call(@manager_name, {:register, username, account_id})
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
  def handle_call({:register, username, account_id}, {from, _tag}, state) do
    case get_key_by_value(state, username) do
      nil ->
        ref = Process.monitor(from)
        {:reply, :ok, Map.put(state, ref, {username, account_id})}

      pid ->
        Logger.error("The PID #{inspect(pid)} is already registered for #{username}")
        {:reply, {:error, :already_registered}, state}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _object, reason}, state) do
    {{username, account_id}, updated_state} = Map.pop!(state, ref)

    Logger.info("#{inspect(username)} is now disconnected (reason: #{inspect(reason)})")
    Logger.info("#{inspect(username)} cleaning state and saving data...")

    # Remove Entity from ECS and notify all Frontends
    {:ok, _events} =
      ElvenGard.ECS.push(
        %PlayerDisconnected{account_id: account_id},
        # Here we don't know the map_ref so we will send the event to a special partition
        partition: :system
      )

    # TODO: Save character in db
    # ...

    Logger.info("#{inspect(username)} cleaning done...")

    {:noreply, updated_state}
  end

  ## Private functions

  defp get_key_by_value(map, value) do
    Enum.find_value(map, fn {key, val} -> if val == value, do: key end)
  end
end
