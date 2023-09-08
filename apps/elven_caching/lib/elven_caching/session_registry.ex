defmodule ElvenCaching.SessionRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import ElvenCaching.Account.Session, only: [is_logged: 1]

  alias ElvenCaching.Account.Session
  alias ElvenGard.ECS.MnesiaBackend.ClusterManager

  @clean_every Application.get_env(:elven_caching, :session_clean_every, 30_000)

  ## Interfaces

  @spec create(map) :: {:ok, Session.t()} | {:error, :already_exists}
  def create(attrs), do: GenServer.call(__MODULE__, {:create, attrs})

  @spec write(Session.t()) :: {:ok, Session.t()}
  def write(session), do: GenServer.call(__MODULE__, {:write, session})

  @spec get(String.t()) :: {:ok, Session.t()} | {:error, :not_found}
  def get(username), do: GenServer.call(__MODULE__, {:get, username})

  @spec delete(String.t()) :: {:ok, Session.t()} | {:error, :not_found}
  def delete(username), do: GenServer.call(__MODULE__, {:delete, username})

  ## Public API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(opts) do
    Logger.info("SessionRegistry starting...")
    {:ok, opts, {:continue, :init_mnesia}}
  end

  @impl true
  def handle_continue(:init_mnesia, opts) do
    ClusterManager.connect_node()
    ClusterManager.create_table!(ElvenCaching.Account.Session)

    :ok = Memento.wait([ElvenCaching.Account.Session])

    if opts[:disable_clean] != true do
      # Autoclean expired keys
      :timer.send_interval(@clean_every, :clean_expired_keys)
    end

    Logger.info("SessionRegistry started")
    {:noreply, nil}
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

  @impl true
  def handle_call({:create, %{username: username} = attrs}, _from, state) do
    fn ->
      case Memento.Query.read(Session, username) do
        nil -> do_create(attrs)
        session when not is_logged(session) -> do_create(attrs)
        _ -> Memento.Transaction.abort(:already_exists)
      end
    end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:write, %Session{} = session}, _from, state) do
    fn -> Memento.Query.write(session) end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:get, username}, _from, state) do
    fn -> Memento.Query.read(Session, username) end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:delete, username}, _from, state) do
    fn ->
      case Memento.Query.read(Session, username) do
        nil ->
          Logger.warn("No character found with username #{username}")
          nil

        character ->
          Memento.Query.delete_record(character)
          character
      end
    end
    |> one()
    |> then(&{:reply, &1, state})
  end

  ## Private functions

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
end
