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

  @typep maybe_ref :: nil | reference
  @typep state :: %{monster_id: pos_integer, moving_ref: maybe_ref}

  ## Public API

  @spec start_link(pos_integer) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(monster_id) do
    GenServer.start_link(__MODULE__, monster_id, name: :"#{__MODULE__}##{monster_id}")
  end

  ## GenServer behaviour

  @impl true
  @spec init(pos_integer) :: {:ok, state}
  def init(monster_id) do
    Logger.debug("MonsterProcess ##{monster_id} started")
    moving_ref = Process.send_after(self(), :move, random_interval())
    {:ok, %{monster_id: monster_id, moving_ref: moving_ref}}
  end

  @impl true
  def handle_info(:move, %{monster_id: monster_id}) do
    move(monster_id)
    new_moving_ref = Process.send_after(self(), :move, random_interval())
    {:noreply, %{monster_id: monster_id, moving_ref: new_moving_ref}}
  end

  ## Private functions

  @spec random_interval() :: integer()
  defp random_interval() do
    500 + :rand.uniform(3_000)
  end

  @spec random_position_in_radius(Monter.t()) :: {x :: integer, y :: integer}
  defp random_position_in_radius(%Monster{base_x: x, base_y: y, movement_radius: r}) do
    # random angle
    alpha = 2 * :math.pi() * :rand.uniform()
    # random radius
    rad = r * :math.sqrt(:rand.uniform())
    # random coordinate
    rand_x = rad * :math.cos(alpha) + x
    rand_y = rad * :math.sin(alpha) + y

    {trunc(rand_x), trunc(rand_y)}
  end

  @spec move(pos_integer) :: any
  defp move(monster_id) do
    {:ok, %Monster{map_id: map_id, map_x: map_x, map_y: map_y} = monster} =
      CachingService.get_monster_by_id(monster_id)

    {:ok, record} = MapRegistry.get_by_id(map_id)
    bin = map_record(record, :bin)
    width = map_record(record, :width)
    height = map_record(record, :height)

    start_pos = {map_x, map_y}
    end_pos = random_position_in_radius(monster)

    case Pathfinding.astar(bin, width, height, start_pos, end_pos) do
      {:error, :no_path} ->
        {:ok, _} = EntityInteractions.sit(monster, true)

      {:ok, cells} ->
        {new_map_x, new_map_y} = Enum.at(cells, -1)
        {:ok, _} = EntityInteractions.move(monster, new_map_x, new_map_y)
    end
  end
end
