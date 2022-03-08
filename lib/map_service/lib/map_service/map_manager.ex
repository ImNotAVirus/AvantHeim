defmodule MapService.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  import ElvenEnums.MapEnums, only: [portal_type: 1]

  @root_path :code.priv_dir(:map_service)

  @default_config [
    lazy: true,
    base_maps_path: Path.join(@root_path, "base_maps")
  ]

  ## Delegates

  ## Public API

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts) do
    name = Keyword.get(opts, :name) || raise ArgumentError, "must supply a name"
    app_config = Application.get_env(:map_service, name, @default_config)
    args_config = Keyword.get(opts, :config, [])
    config = Keyword.merge(app_config, args_config)

    check_config!(name, config)

    state = config |> Enum.into(%{}) |> Map.put(:name, name)
    GenServer.start_link(__MODULE__, state, name: name)
  end

  ## GenServer behaviour

  @impl true
  def init(%{name: name, lazy: lazy} = state) do
    if lazy do
      Logger.info("#{inspect(name)} started in lazy mode")
      {:ok, state}
    else
      Logger.info("#{inspect(name)} starting...")
      {:ok, state, {:continue, :init_base_maps}}
    end
  end

  @impl true
  def handle_continue(:init_base_maps, %{name: name} = state) do
    map_info = parse_maps_info!(state)

    Logger.info("#{inspect(name)} is now monitoring #{length(map_info)} maps")

    {:noreply, state}
  end

  ## Private functions

  defp check_config!(name, config) do
    valid_keys = Keyword.keys(@default_config)

    Enum.each(config, fn {key, value} ->
      if key not in valid_keys,
        do: raise(ArgumentError, "unknown config #{key}: #{value} for #{inspect(name)}")
    end)
  end

  defp parse_maps_info!(%{base_maps_path: base_maps_path}) do
    map_dirs = Path.wildcard("#{base_maps_path}/*")

    Enum.map(map_dirs, fn dir ->
      id = dir |> Path.basename() |> String.to_integer()
      Logger.debug("Parsing map##{id}: #{Path.relative_to_cwd(dir)}")

      "#{dir}/*.{yml,yaml}"
      |> Path.wildcard()
      |> Enum.map(&YamlElixir.read_from_file!/1)
      |> Enum.map(&parse_map_config(&1, id, dir))
      |> List.flatten()
      |> Enum.into(%{})
      |> Map.put(:id, id)
    end)
  end

  defp parse_map_config(config, id, dir) do
    Enum.map(config, fn
      {"map_music_id", music_id} -> [music_id: music_id]
      {"map_vnum", vnum} -> [map_vnum: vnum]
      {"grid_file", grid_file} -> parse_map_grid!(id, dir, grid_file)
      {"portals", portals} -> parse_map_portals!(portals, id)
      {"npcs", _npcs} -> [npcs: []]
    end)
  end

  defp parse_map_grid!(id, dir, grid_filename) do
    filename = Path.join(dir, grid_filename)

    <<width::16-little, height::16-little, map_grid::binary>> = File.read!(filename)
    total_size = width * height
    <<_::bytes-size(total_size)>> = map_grid

    Logger.debug("Map parsed: id=#{id} size=#{width}x#{height}")

    [width: width, height: height, grid: map_grid]
  end

  defp parse_map_portals!(portals, id) do
    [portals: Enum.map(portals, &parse_map_portal!(&1, id))]
  end

  defp parse_map_portal!(portals, map_id) do
    portals
    |> Enum.map(fn
      {"destination_map_id", destination_map_id} -> {:destination_map_id, destination_map_id}
      {"destination_map_x", destination_map_x} -> {:destination_map_x, destination_map_x}
      {"destination_map_y", destination_map_y} -> {:destination_map_y, destination_map_y}
      {"source_map_id", source_map_id} -> {:source_map_id, source_map_id}
      {"source_map_x", source_map_x} -> {:source_map_x, source_map_x}
      {"source_map_y", source_map_y} -> {:source_map_y, source_map_y}
      {"type", type} -> {:type, portal_type_to_enum(type, map_id)}
    end)
    |> Enum.into(%{})
  end

  defp portal_type_to_enum(type, map_id) do
    atom = String.to_existing_atom(type)

    if atom not in portal_type(:__keys__) do
      raise "unknown portal type #{inspect(atom)} for map #{map_id}"
    end

    atom
  end
end
