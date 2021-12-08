defmodule CachingService.SessionRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.Player.Session, only: [is_logged: 1]

  alias CachingService.Player.Session

  @typep maybe_session :: Session.t() | nil
  @typep session_result :: {:ok, maybe_session()} | {:error, any}

  @session_registry __MODULE__

  ## Public API

  @spec start_link(keyword) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @session_registry)
  end

  @typep maybe_encryption_key :: pos_integer | nil
  @spec create_session(String.t(), String.t(), maybe_encryption_key()) ::
          {:ok, Session.t()} | {:error, any}
  def create_session(username, password, key \\ nil) do
    case get_session_by_username(username) do
      {:ok, nil} -> do_create_session(username, password, key)
      {:ok, session} when not is_logged(session) -> do_create_session(username, password, key)
      {:ok, _} -> {:error, :already_exists}
      {:error, _} = e -> e
    end
  end

  @spec get_session_by_username(String.t()) :: session_result()
  def get_session_by_username(username) do
    Memento.transaction(fn -> Memento.Query.read(Session, username) end)
  end

  ## GenServer behaviour

  @impl true
  @spec init(nil) :: {:ok, nil}
  def init(nil) do
    Memento.Table.create!(CachingService.Player.Session)
    :ok = Memento.wait([CachingService.Player.Session])
    Logger.debug("SessionRegistry started")
    {:ok, nil}
  end

  ## Private functions

  @spec do_create_session(String.t(), String.t(), maybe_encryption_key()) ::
          {:ok, Session.t()} | {:error, any}
  defp do_create_session(username, password, key) do
    Memento.transaction(fn ->
      Session.new(username, password, key) |> Memento.Query.write()
    end)
  end
end
