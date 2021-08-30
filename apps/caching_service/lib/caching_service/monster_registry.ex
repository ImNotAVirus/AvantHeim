defmodule CachingService.MonsterRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias CachingService.MonsterRegistry.{MapMonster, MonsterProcess}

  @supervisor_name CachingService.MonstersSupervisor
  @monsters_filename "#{:code.priv_dir(:caching_service)}/client_files/monsters.csv"

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("MonsterRegistry starting...")
    {:ok, nil, {:continue, :init_monsters}}
  end

  @impl true
  def handle_continue(:init_monsters, state) do
    res =
      @monsters_filename
      |> File.read!()
      |> String.split("\r\n", trim: true)
      |> Stream.with_index(1)
      |> Stream.map(fn {x, i} -> MapMonster.from_binary(x, i) end)
      |> Enum.map(&start_monster_process/1)

    Logger.info("MonsterRegistry started with #{length(res)} monsters")

    {:noreply, state}
  end

  ## Private function

  defp start_monster_process(monster) do
    spec = {MonsterProcess, monster}
    {:ok, _} = DynamicSupervisor.start_child(@supervisor_name, spec)
    :ok
  end
end
