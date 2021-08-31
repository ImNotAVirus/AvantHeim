defmodule CachingService.MonsterRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias CachingService.Map.Monster
  alias CachingService.MonsterRegistry.MonsterProcess

  @supervisor_name CachingService.MonstersSupervisor
  @monsters_filename "#{:code.priv_dir(:caching_service)}/client_files/monsters.csv"

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec write_monster(Monster.t()) :: {:ok, Monster.t()} | {:error, any}
  def write_monster(%Monster{} = monster) do
    Memento.transaction(fn -> Memento.Query.write(monster) end)
  end

  @spec get_monster_by_id(pos_integer) :: {:ok, Monster.t()} | {:error, any}
  def get_monster_by_id(id) do
    Memento.transaction(fn -> Memento.Query.read(Monster, id) end)
  end

  @spec get_monsters_by_map_id(pos_integer) :: {:ok, [Monster.t()]} | {:error, any}
  def get_monsters_by_map_id(map_id) do
    guards = {:==, :map_id, map_id}
    Memento.transaction(fn -> Memento.Query.select(Monster, guards) end)
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("MonsterRegistry starting...")

    Memento.Table.create!(CachingService.Map.Monster)
    :ok = Memento.wait([CachingService.Map.Monster])

    {:ok, nil, {:continue, :init_monsters}}
  end

  @impl true
  def handle_continue(:init_monsters, state) do
    res =
      @monsters_filename
      |> File.read!()
      |> String.split("\r\n", trim: true)
      |> Stream.with_index(1)
      |> Stream.map(fn {x, i} -> Monster.from_binary(x, i) end)
      |> Stream.map(&start_monster_process/1)
      |> Enum.map(&persist_monster/1)

    Logger.info("MonsterRegistry started with #{length(res)} monsters")

    {:noreply, state}
  end

  ## Private function

  defp start_monster_process(%Monster{id: monster_id} = monster) do
    spec = {MonsterProcess, monster_id}
    {:ok, _} = DynamicSupervisor.start_child(@supervisor_name, spec)
    monster
  end

  defp persist_monster(monster) do
    {:ok, _} = write_monster(monster)
    :ok
  end
end
