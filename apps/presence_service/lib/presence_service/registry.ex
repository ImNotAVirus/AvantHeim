defmodule PresenceService.Registry do
  @moduledoc """
  TODO: PresenceService.Registry
  """

  use GenServer

  require Logger

  @name {:global, __MODULE__}

  ## Public API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @name)
  end

  def track(pid, account_id, account_name) do
    GenServer.call(@name, {:track, pid, account_id, account_name})
  end

  def get_session_by_id(account_id) do
    GenServer.call(@name, {:get_session_by_id, account_id})
  end

  def get_session_by_name(account_name) do
    GenServer.call(@name, {:get_session_by_name, account_name})
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    state = %{
      # Account id => session_pid
      id_mapping: %{},
      # Account name => session_pid
      name_mapping: %{},
      # monitor_ref => {account_id, account_name}
      ref_mapping: %{}
    }

    Logger.info("#{inspect(__MODULE__)} started")

    {:ok, state}
  end

  @impl true
  def handle_call({:track, pid, account_id, account_name}, _from, state) do
    with {:id, nil} <- {:id, state[:id_mapping][account_id]},
         {:name, nil} <- {:name, state[:name_mapping][account_name]} do
      ref = Process.monitor(pid)

      state
      |> put_in([:id_mapping, account_id], pid)
      |> put_in([:name_mapping, account_name], pid)
      |> put_in([:ref_mapping, ref], {account_id, account_name})
      |> then(&{:ok, &1})
    else
      _ -> {{:error, :already_exists}, state}
    end
    |> then(&{:reply, elem(&1, 0), elem(&1, 1)})
  end

  @impl true
  def handle_info({:DOWN, ref, :process, object, _reason}, state) do
    case Map.get(state.ref_mapping, ref) do
      {account_id, account_name} ->
        do_down({account_id, account_name}, ref, state)

      nil ->
        Logger.critical(fn ->
          id_links =
            state.id_mapping |> Enum.filter(&(elem(&1, 1) == object)) |> Enum.map(&elem(&1, 0))

          name_links =
            state.name_mapping |> Enum.filter(&(elem(&1, 1) == object)) |> Enum.map(&elem(&1, 0))

          "An unknown user has be disconnected: ref:#{inspect(ref)} " <>
            "account_id links: #{inspect(id_links)} " <>
            "account_name links: #{inspect(name_links)} "
        end)

        state
    end
    |> then(&{:noreply, &1})
  end

  ## Helpers

  defp do_down(session, ref, state) do
    {account_id, account_name} = session

    :ok = ElvenPubSub.broadcast(ElvenPubSub.Topics.session(), {:disconnected, session})

    state
    |> Map.update!(:id_mapping, &Map.delete(&1, account_id))
    |> Map.update!(:name_mapping, &Map.delete(&1, account_name))
    |> Map.update!(:ref_mapping, &Map.delete(&1, ref))
  end
end
