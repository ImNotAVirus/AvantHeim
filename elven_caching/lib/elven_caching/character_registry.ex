defmodule ElvenCaching.CharacterRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias ElvenCaching.Entity.Character
  alias ElvenCaching.MnesiaClusterManager

  ## Interfaces

  @spec create(map) :: {:ok, Character.t()}
  def create(attrs), do: GenServer.call(__MODULE__, {:create, attrs})

  @spec write(Character.t()) :: {:ok, Character.t()}
  def write(character), do: GenServer.call(__MODULE__, {:write, character})

  @spec get(pos_integer) :: {:ok, Character.t()} | {:error, :not_found}
  def get(id), do: GenServer.call(__MODULE__, {:get, id})

  @spec get_by_name(String.t()) :: {:ok, Character.t()} | {:error, :not_found}
  def get_by_name(name), do: GenServer.call(__MODULE__, {:get_by_name, name})

  @spec delete_by_account_id(pos_integer) :: {:ok, Character.t()} | {:error, :not_found}
  def delete_by_account_id(account_id),
    do: GenServer.call(__MODULE__, {:delete_by_account_id, account_id})

  @spec get_by_map_id(pos_integer, [tuple]) :: {:ok, [Character.t()]} | {:error, any}
  def get_by_map_id(map_id, except_guards \\ []),
    do: GenServer.call(__MODULE__, {:get_by_map_id, map_id, except_guards})

  ## Public API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("CharacterRegistry starting...")
    {:ok, nil, {:continue, :init_mnesia}}
  end

  @impl true
  def handle_continue(:init_mnesia, nil) do
    MnesiaClusterManager.connect_node()
    MnesiaClusterManager.create_table!(ElvenCaching.Entity.Character)

    :ok = Memento.wait([ElvenCaching.Entity.Character])

    Logger.info("CharacterRegistry started")
    {:noreply, nil}
  end

  @impl true
  def handle_call({:create, attrs}, _from, state) do
    fn -> attrs |> Character.new() |> Memento.Query.write() end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:write, %Character{} = character}, _from, state) do
    fn -> Memento.Query.write(character) end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:get, id}, _from, state) do
    fn -> Memento.Query.read(Character, id) end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:get_by_name, name}, _from, state) do
    fn -> Memento.Query.select(Character, {:==, :name, name}) end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:delete_by_account_id, account_id}, _from, state) do
    fn ->
      case Memento.Query.select(Character, {:==, :account_id, account_id}) do
        [character] ->
          Memento.Query.delete_record(character)
          character

        [] ->
          Logger.warn("No character found with account id #{account_id}")
          nil
      end
    end
    |> one()
    |> then(&{:reply, &1, state})
  end

  def handle_call({:get_by_map_id, map_id, except_guards}, _from, state) do
    guards = [{:==, :map_id, map_id} | List.wrap(except_guards)]

    fn -> Memento.Query.select(Character, guards) end
    |> Memento.transaction()
    |> then(&{:reply, &1, state})
  end

  ## Private functions

  defp one(query) do
    case Memento.transaction(query) do
      {:ok, value} when is_struct(value) -> {:ok, value}
      {:ok, [value]} -> {:ok, value}
      {:ok, []} -> {:error, :not_found}
      {:ok, nil} -> {:error, :not_found}
    end
  end
end
