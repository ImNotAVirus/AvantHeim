defmodule LoginService.SessionManager do
  @moduledoc """
  TODO: Documentation for LoginService.SessionManager
  """

  use GenServer

  require Logger

  alias LoginService.SessionManager.Session

  @name {:global, __MODULE__}

  ## Public API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  def create_session(attrs) do
    GenServer.call(@name, {:create_session, attrs})
  end

  ## GenServer Behaviour

  @impl true
  def init(_opts) do
    _ = schedule_clean()

    # %{account_name => inserted_at} or %{account_name => pid}
    {:ok, %{}}
  end

  @impl true
  def handle_info(:clean_expired_keys, state) do
    now = now()
    ttl = ttl()
    original_size = map_size(state)

    new_state =
      state
      |> Map.to_list()
      |> Enum.filter(fn {_key, inserted_at} -> is_integer(inserted_at) end)
      |> Enum.filter(fn {_key, inserted_at} -> inserted_at + ttl <= now end)
      |> Enum.reduce(state, &Map.delete(&2, elem(&1, 0)))

    Logger.debug("Auto purge sessions: cleared #{map_size(state) - original_size} session(s)")
    _ = schedule_clean()

    {:noreply, new_state}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, pid, reason}, state) do
    session_tuple = state |> Map.to_list() |> Enum.find(&(elem(&1, 1).monitor == ref))

    if reason != :normal do
      Logger.warn("session exited with reason #{inspect(reason)}")
    end

    case session_tuple do
      {username, _session} ->
        Logger.debug("#{username} is now disconnected")
        {:noreply, Map.delete(state, username)}

      nil ->
        Logger.error("no session found with ref #{inspect(ref)} and pid #{inspect(pid)}")
        {:noreply, state}
    end
  end

  @impl true
  def handle_call({:create_session, attrs}, _from, state) do
    username = Map.fetch!(attrs, :username)

    case state do
      %{^username => %Session{monitor: ref}} when is_reference(ref) ->
        {:reply, {:error, :already_exists}, state}

      _ ->
        session = Session.new(attrs)
        {:reply, {:ok, session}, Map.put(state, username, session)}
    end
  end

  @impl true
  def handle_call({:authenticate, username, password}, {pid, _ref}, state) do
    hash = :crypto.hash(:sha512, password) |> Base.encode16()

    case state do
      %{^username => %Session{monitor: nil, password: ^hash} = session} ->
        ref = Process.monitor(pid)
        new_session = %Session{session | monitor: ref}
        {:reply, {:ok, new_session}, Map.put(state, username, new_session)}

      _ ->
        {:reply, {:error, :invalid_session}, state}
    end
  end

  ## Helpers

  defp now(), do: :erlang.monotonic_time(:millisecond)
  defp ttl(), do: Application.get_env(:login_service, :session_ttl, :timer.seconds(120))
  defp clean_every(), do: Application.get_env(:login_service, :clean_interval, :timer.seconds(30))

  defp schedule_clean() do
    Process.send_after(self(), :clean_expired_keys, clean_every())
  end
end
