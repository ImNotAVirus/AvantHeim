defmodule CachingService.MapRegistry do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import CachingService.MapRegistry.MapRecord

  @table_name :map_registry
  @maps_path "#{:code.priv_dir(:caching_service)}/client_files/maps"

  ## Public API

  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec get_by_id(pos_integer) :: {:ok, any} | {:error, :not_found}
  def get_by_id(map_id) do
    case :ets.lookup(@table_name, map_id) do
      [{^map_id, map}] -> {:ok, map}
      _ -> {:error, :not_found}
    end
  end

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("MapRegistry starting...")
    {:ok, nil, {:continue, :init_maps}}
  end

  @impl true
  def handle_continue(:init_maps, nil) do
    :ets.new(@table_name, [:set, :protected, :named_table])

    map_files = Path.wildcard("#{@maps_path}/*")

    map_files
    |> Stream.map(&parse_map_file/1)
    |> Enum.each(&persist_map/1)

    Logger.info("MapRegistry started with #{length(map_files)} maps")

    {:noreply, map_files}
  end

  ## Private functions

  defp parse_map_file(filename) do
    Logger.debug("Parsing map file: #{filename}")

    id = filename |> Path.basename() |> String.to_integer(10)

    <<width::16-little, height::16-little, map_bin::binary>> = File.read!(filename)
    total_size = width * height
    <<_::bytes-size(total_size)>> = map_bin

    Logger.debug("Map parsed: id=#{id} size=#{width}x#{height}")

    map_record(
      id: id,
      width: width,
      height: height,
      bin: map_bin
    )
  end

  defp persist_map(record) do
    id = map_record(record, :id)
    true = :ets.insert_new(@table_name, {id, record})
  end
end
