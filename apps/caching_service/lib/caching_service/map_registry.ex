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

  ## GenServer behaviour

  @impl true
  def init(_) do
    Logger.info("MapRegistry starting...")
    {:ok, nil, {:continue, :init_map}}
  end

  @impl true
  def handle_continue(:init_map, nil) do
    :ets.new(@table_name, [:set, :protected, :named_table])

    map_files = Path.wildcard("#{@maps_path}/*")

    map_files
    |> Stream.map(&parse_map_file/1)
    |> Enum.each(&persist_map/1)

    Logger.info("MapRegistry started")

    {:noreply, map_files}
  end

  ## Private functions

  defp parse_map_file(filename) do
    Logger.debug("Parsing map file: #{filename}")

    id = filename |> Path.basename() |> String.to_integer(10)

    <<width::16-little, height::16-little, map_bin::binary>> = File.read!(filename)

    tensor =
      map_bin
      |> Nx.from_binary({:s, 8})
      |> Nx.reshape({height, width}, names: [:height, :width])

    Logger.debug("Map parsed: id=#{id} size=#{width}x#{height}")

    map_record(
      id: id,
      width: width,
      height: height,
      tensor: tensor
    )
  end

  defp persist_map(map_tuple) do
    true = :ets.insert_new(@table_name, map_tuple)
  end
end
