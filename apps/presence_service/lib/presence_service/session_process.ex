defmodule PresenceService.SessionProcess do
  @moduledoc """
  TODO: PresenceService.SessionProcess

  FSM:

  init
  init -> init_timeout
  init -> authenticated
  authenticated -> in_game

  """

  alias PresenceService.Session

  @behaviour :gen_statem

  ## Public API

  @doc false
  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, List.wrap(opts)},
      type: :worker,
      restart: :temporary,
      shutdown: 500
    }
  end

  def start_link(%Session{} = session, opts \\ []) do
    :gen_statem.start_link(__MODULE__, {session, opts}, [])
  end

  def authenticate(pid) do
    :gen_statem.cast(pid, :authenticate)
  end

  ## gen_statem behaviour

  @impl true
  def init({session, opts}) do
    init_timeout = Keyword.get(opts, :init_timeout, default_init_timeout())
    {:ok, :init, session, {:state_timeout, init_timeout, :init_timeout}}
  end

  @impl true
  def callback_mode(), do: :handle_event_function

  @impl true
  # init -> init_timeout
  def handle_event(:state_timeout, :init_timeout, :init, _data) do
    {:stop, :normal}
  end

  # init -> authenticate
  def handle_event(:cast, :authenticate, :init, data) do
    {:next_state, :authenticated, data}
  end

  ## Helpers

  defp default_init_timeout(), do: Application.get_env(:presence_service, :session_ttl, 120)
end
