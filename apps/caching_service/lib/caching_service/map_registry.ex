defmodule CachingService.MapRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.MapRegistry.MapRecord

  alias CachingService.MapRegistry.MapRecord

  @map_registry __MODULE__
  @table_name :maps

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: @map_registry)
  end

  @spec insert_new(MapRecord.t()) :: {:ok, MapRecord.t()} | {:error, :cant_insert}
  def insert_new(record) do
    GenServer.call(@map_registry, {:insert_new, record})
  end

  @spec get_by_id(pos_integer) :: {:ok, MapRecord.t()} | {:error, :not_found}
  def get_by_id(map_id) do
    case :ets.lookup(@table_name, map_id) do
      [{^map_id, map}] -> {:ok, map}
      _ -> {:error, :not_found}
    end
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    :ets.new(@table_name, [:set, :protected, :named_table])
    Logger.info("MapRegistry started...")
    {:ok, nil}
  end

  @impl true
  def handle_call({:insert_new, record}, _from, state) do
    id = map_record(record, :id)

    result =
      case :ets.insert_new(@table_name, {id, record}) do
        true -> {:ok, record}
        false -> {:error, :cant_insert}
      end

    {:reply, result, state}
  end
end
