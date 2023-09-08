defmodule ElvenGard.ECS.MnesiaBackend.ClusterManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  @type storage_type :: :ram_copies | :disc_copies | :disc_only_copies

  @retry_after 1_000

  ## Public API

  @spec start_link(GenServer.options()) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec connect_node(storage_type()) :: :ok
  def connect_node(copy_type \\ :ram_copies) do
    GenServer.call(__MODULE__, {:connect_node, copy_type})
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
  def init(opts) do
    state = %{retry: Keyword.get(opts, :retry, false)}

    case Keyword.get(opts, :auto_connect, true) do
      true -> {:ok, state, {:continue, :connect_node}}
      false -> {:ok, state}
    end
  end

  @impl true
  def handle_continue(:connect_node, %{retry: retry} = state) do
    do_connect_node(:ram_copies, retry)
    {:noreply, state}
  end

  @impl true
  def handle_call({:connect_node, copy_type}, _from, %{retry: retry} = state) do
    {:reply, do_connect_node(copy_type, retry), state}
  end

  def handle_call({:request_join, slave, copy_type}, _from, state) do
    Logger.info("request_join slave: #{inspect(slave)} - copy_type: #{inspect(copy_type)}")

    # Add an extra node to Mnesia
    {:ok, _} = :mnesia.change_config(:extra_db_nodes, [slave])

    # Copy all tables on the slave
    tables = :mnesia.system_info(:tables)
    Enum.map(tables, &:mnesia.add_table_copy(&1, slave, copy_type))
    {:reply, :ok, state}
  end

  ## Helpers

  defp do_connect_node(copy_type, retry) do
    case Node.list() do
      [] ->
        if not retry do
          Logger.info("connect_node no node found, no retry")
        else
          Logger.info("connect_node no node found, retry in #{@retry_after}ms")
          Process.sleep(@retry_after)
          do_connect_node(copy_type, retry)
        end

      [master | _] ->
        Logger.info("connect_node master: #{inspect(master)} - copy_type: #{inspect(copy_type)}")
        GenServer.multi_call([master], __MODULE__, {:request_join, node(), copy_type})
        :ok
    end
  end
end
