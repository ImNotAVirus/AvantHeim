defmodule CachingService.SessionRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.Account.Session, only: [is_logged: 1]

  alias CachingService.Account.Session

  @clean_every Application.get_env(:caching_service, :session_clean_every, 30_000)

  ## Interfaces

  @spec create(map) :: {:ok, Session.t()} | {:error, any}
  def create(%{username: username} = attrs) do
    one(fn ->
      case Memento.Query.read(Session, username) do
        nil -> do_create(attrs)
        session when not is_logged(session) -> do_create(attrs)
        _ -> Memento.Transaction.abort(:already_exists)
      end
    end)
  end

  @spec write(Session.t()) :: {:ok, Session.t()}
  def write(%Session{} = session) do
    one(fn -> Memento.Query.write(session) end)
  end

  @spec get(String.t()) :: {:ok, Session.t()} | {:error, :not_found}
  def get(username) do
    one(fn -> Memento.Query.read(Session, username) end)
  end

  @spec delete(String.t()) :: :ok
  def delete(username) do
    one(fn ->
      case Memento.Query.read(Session, username) do
        nil ->
          Logger.warn("No character found with username #{username}")
          nil

        character ->
          Memento.Query.delete_record(character)
          character
      end
    end)
  end

  ## Interfaces helpers

  defp one(query) do
    case Memento.transaction(query) do
      {:ok, value} when is_struct(value) -> {:ok, value}
      {:ok, [value]} -> {:ok, value}
      {:ok, []} -> {:error, :not_found}
      {:ok, nil} -> {:error, :not_found}
      {:error, {:transaction_aborted, error}} -> {:error, error}
    end
  end

  defp do_create(attrs) do
    attrs |> Session.new() |> Memento.Query.write()
  end

  ## Public API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(nil) do
    Memento.Table.create!(CachingService.Account.Session)
    :ok = Memento.wait([CachingService.Account.Session])

    # Autoclean expired keys
    :timer.send_interval(@clean_every, :clean_expired_keys)

    Logger.info("SessionRegistry started")
    {:ok, nil}
  end

  @impl true
  def handle_info(:clean_expired_keys, state) do
    now = Session.ttl_to_expire(0)

    :ok =
      Memento.transaction(fn ->
        Session
        |> Memento.Query.select({:<, :expire, now})
        |> Enum.map(&Memento.Query.delete_record/1)
        |> then(&Logger.debug("Auto purge sessions: cleared #{length(&1)} record(s)"))
      end)

    {:noreply, state}
  end
end
