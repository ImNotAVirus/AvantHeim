defmodule CachingService.MonsterRegistry.MonsterProcess do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.MapRegistry.MapRecord, only: [map_record: 2]

  alias Algorithms.Pathfinding
  alias CachingService.MapRegistry
  alias CachingService.Map.Monster

  ## Public API

  @spec start_link(pos_integer) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(monster_id) do
    GenServer.start_link(__MODULE__, monster_id, name: :"#{__MODULE__}##{monster_id}")
  end

  ## GenServer behaviour

  @impl true
  def init(monster_id) do
    Logger.debug("MonsterProcess ##{monster_id} started")
    Process.send_after(self(), :move2, 1000)
    {:ok, monster_id}
  end

  def handle_info(:move2, monster_id) do
    Pathfinding.astar(
      <<0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0>>,
      5,
      5,
      {0, 0},
      {4, 4}
    )

    {:ok, record} = MapRegistry.get_by_id(1)
    [m] = :mnesia.ets(fn -> :ets.lookup({CachingService.Map.Monster, monster_id}) end)
    %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = monster = Memento.Query.Data.load(m)

    Process.send_after(self(), :move2, 1000)
    {:noreply, monster_id}
  end

  @impl true
  def handle_info(:move, monster_id) do
    # {:ok, %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = monster} =
    #   CachingService.get_monster_by_id(monster_id)

    [m] = :mnesia.ets(fn -> :mnesia.read({CachingService.Map.Monster, monster_id}) end)
    %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = monster = Memento.Query.Data.load(m)

    # %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = 
    #   monster = Monster.from_binary("1761,f,t,1,103,14,25,,2", monster_id)

    {:ok, record} = MapRegistry.get_by_id(map_id)
    bin = map_record(record, :bin)
    width = map_record(record, :width)
    height = map_record(record, :height)

    start_pos = {map_x, map_y}
    end_pos = select_position_in_radius(monster)

    Pathfinding.astar(bin, width, height, start_pos, end_pos)
    # |> IO.inspect()

    Process.send_after(self(), :move, 1000)

    {:noreply, monster_id}
  end

  ## Private functions

  defp select_position_in_radius(%Monster{base_x: x, base_y: y, movement_radius: r}) do
    # random angle
    alpha = 2 * :math.pi() * :rand.uniform()
    # random radius
    rad = r * :math.sqrt(:rand.uniform())
    # random coordinate
    rand_x = rad * :math.cos(alpha) + x
    rand_y = rad * :math.sin(alpha) + y

    {trunc(rand_x), trunc(rand_y)}
  end
end
