defmodule SessionService.Worker do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias SessionService.{Session, Sessions}

  @clean_every 30_000

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## Interfaces

  @spec create_session(String.t(), String.t()) :: {:ok, Session.t()} | {:error, any}
  def create_session(username, password) do
    GenServer.call(__MODULE__, {:create_session, username, password})
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("SessionService starting...")
    {:ok, nil, {:continue, :init_ets}}
  end

  @impl true
  def handle_continue(:init_ets, nil) do
    ets_ctx = Sessions.init_ets()

    # Autoclean expired keys
    :timer.send_interval(@clean_every, :clean_expired_keys)

    Logger.info("SessionManager started")
    {:noreply, ets_ctx}
  end

  @impl true
  def handle_call({:create_session, username, password}, _from, state) do
    maybe_session = Sessions.get_by_username(username, state)

    if is_nil(maybe_session) or not Session.in_channel?(maybe_session) do
      new_session = Sessions.create(username, password, state)
      {:reply, {:ok, new_session}, state}
    else
      {:reply, {:error, :already_connected}, state}
    end
  end

  @impl true
  def handle_info(:clean_expired_keys, state) do
    Sessions.clean_expired_keys(state)
    {:noreply, state}
  end
end
