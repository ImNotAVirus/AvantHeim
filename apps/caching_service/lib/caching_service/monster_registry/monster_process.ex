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
  alias ChannelEndpoint.Endpoint.EntityInteractions

  ## Public API

  @spec start_link(pos_integer) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(monster_id) do
    GenServer.start_link(__MODULE__, monster_id, name: :"#{__MODULE__}##{monster_id}")
  end

  ## GenServer behaviour

  @impl true
  def init(monster_id) do
    Logger.debug("MonsterProcess ##{monster_id} started")
    # Process.send_after(self(), :move, 1000)
    send(self(), :move)
    {:ok, monster_id}
  end

  @impl true
  def handle_info(:move, monster_id) do
    case :random.uniform() do
      x when x > 0.50 -> move(monster_id)
      _ -> sit(monster_id)
    end

    Process.send_after(self(), :move, random_interval())

    {:noreply, monster_id}
  end

  ## Private functions

  @spec random_interval() :: integer()
  defp random_interval() do
    3_500 + :rand.uniform(3_000)
  end

  @spec select_position_in_radius(Monter.t()) :: {integer, integer}
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

  defp sit(monster_id) do
    {:ok, %Monster{} = monster} = CachingService.get_monster_by_id(monster_id)

    if not monster.is_sitting do
      {:ok, _} = EntityInteractions.sit(monster, true)
    end
  end

  defp move(monster_id) do
    {:ok, %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = monster} =
      CachingService.get_monster_by_id(monster_id)

    {:ok, record} = MapRegistry.get_by_id(map_id)
    bin = map_record(record, :bin)
    width = map_record(record, :width)
    height = map_record(record, :height)

    start_pos = {map_x, map_y}
    end_pos = select_position_in_radius(monster)

    case Pathfinding.astar(bin, width, height, start_pos, end_pos) do
      {:error, :no_path} ->
        {:ok, _} = EntityInteractions.sit(monster, true)

      {:ok, cells} ->
        {new_map_x, new_map_y} = Enum.at(cells, -1)
        {:ok, _} = EntityInteractions.move(monster, new_map_x, new_map_y)
    end
  end
end
