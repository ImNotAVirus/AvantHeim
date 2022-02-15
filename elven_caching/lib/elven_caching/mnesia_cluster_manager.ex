defmodule ElvenCaching.MnesiaClusterManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  @type storage_type :: :ram_copies | :disc_copies | :disc_only_copies

  ## Public API

  @spec start_link(GenServer.options()) :: GenServer.on_start()
  def start_link(_opts) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  @spec connect_node(storage_type()) :: :ok
  def connect_node(copy_type \\ :ram_copies) do
    case Node.list() do
      [] -> :ok
      [master | _] -> GenServer.call(__MODULE__, {:connect_node, master, copy_type})
    end
  end

  @spec create_table!(module, Keyword.t()) :: :ok
  def create_table!(table, opts \\ []) do
    case Memento.Table.create(table, opts) do
      :ok -> :ok
      {:error, {:already_exists, ^table}} -> :ok
      error -> raise "can't create table: #{inspect(error)}"
    end
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    {:ok, nil}
  end

  @impl true
  def handle_call({:connect_node, master, copy_type}, _from, state) do
    GenServer.multi_call([master], __MODULE__, {:request_join, node(), copy_type})
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:request_join, slave, copy_type}, _from, state) do
    # Add an extra node to Mnesia
    {:ok, _} = :mnesia.change_config(:extra_db_nodes, [slave])

    # Copy all tables on the slave
    tables = :mnesia.system_info(:tables)
    Enum.map(tables, &:mnesia.add_table_copy(&1, slave, copy_type))
    {:reply, :ok, state}
  end
end
