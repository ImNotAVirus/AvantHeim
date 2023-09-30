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
      start: {__MODULE__, :start_link, [opts]},
      restart: :temporary
    }
  end

  def start_link(%Session{} = session) do
    start_link({session, []})
  end

  def start_link({%Session{} = session, opts}) do
    :gen_statem.start_link(__MODULE__, {session, opts}, [])
  end

  def link(process, pid) do
    :gen_statem.cast(process, {:link, pid})
  end

  ## gen_statem behaviour

  @impl true
  def callback_mode(), do: :state_functions

  @impl true
  def init({session, opts}) do
    init_timeout = Keyword.get(opts, :init_timeout, default_init_timeout())
    {:ok, :init, {session, nil}, {:state_timeout, init_timeout, :init_timeout}}
  end

  # init -> init_timeout
  def init(:state_timeout, :init_timeout, _session) do
    {:stop, :normal}
  end

  # init -> linked
  def init(:cast, {:link, pid}, session) do
    Process.link(pid)
    {:next_state, :linked, session}
  end

  ## Helpers

  defp default_init_timeout(), do: Application.get_env(:presence_service, :session_ttl, 120)
end
