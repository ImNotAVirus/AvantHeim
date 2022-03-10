defmodule MapService.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias MapService.ConfigFile.MapConfig

  @type map_config :: map

  @root_path :code.priv_dir(:map_service)

  @default_config [
    lazy: true,
    base_maps_path: Path.join(@root_path, "base_maps")
  ]

  ## Delegates

  ## Public API

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    app_config = Application.get_env(:map_service, name, [])
    args_config = Keyword.get(opts, :config, [])
    config = @default_config |> Keyword.merge(app_config) |> Keyword.merge(args_config)

    check_config!(name, config)

    state = config |> Enum.into(%{}) |> Map.put(:name, name)

    GenServer.start_link(__MODULE__, state, name: name)
  end

  @spec start_base_map(non_neg_integer) :: {:ok, map_config()}
  @spec start_base_map(atom | pid, non_neg_integer) :: {:ok, map_config()}
  def start_base_map(manager \\ __MODULE__, map_vnum) do
    GenServer.call(manager, {:start_base_map, map_vnum})
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
  def handle_continue(:init_base_maps, %{base_maps_path: base_maps_path, name: name} = state) do
    map_dirs = Path.wildcard("#{base_maps_path}/*")
    map_vnums = Enum.map(map_dirs, &(&1 |> Path.basename() |> String.to_integer()))

    Enum.each(map_vnums, &do_start_base_map(&1, state))

    Logger.info("#{inspect(name)} is now monitoring #{length(map_vnums)} maps")

    {:noreply, state}
  end

  @impl true
  def handle_call({:start_base_map, map_vnum}, _from, state) do
    {:reply, do_start_base_map(map_vnum, state), state}
  end

  ## Private functions

  defp check_config!(name, config) do
    valid_keys = Keyword.keys(@default_config)

    Enum.each(config, fn {key, value} ->
      if key not in valid_keys,
        do: raise(ArgumentError, "unknown config #{key}: #{value} for #{inspect(name)}")
    end)
  end

  defp do_start_base_map(vnum, state) do
    config = get_map_config(vnum, state)

    # TODO: Start map process with config
    # TODO: Monitor map process

    {:ok, config}
  end

  defp get_map_config(vnum, %{base_maps_path: base_maps_path}) do
    dir = Path.join(base_maps_path, Integer.to_string(vnum))

    Logger.debug("Parsing map##{vnum}: #{Path.relative_to_cwd(dir)}")

    "#{dir}/*.{yml,yaml}"
    |> Path.wildcard()
    |> Enum.map(&YamlElixir.read_from_file!(&1, atoms: true))
    |> Enum.reduce(%{}, fn element, acc -> Map.merge(acc, element, &merger/3) end)
    |> Map.put("map_id", vnum)
    |> Map.put("map_dir", dir)
    |> MapConfig.new()
    |> tap(&debug/1)
  end

  defp merger("portals", v1, v2), do: Enum.concat(v1, v2)
  defp merger("npcs", v1, v2), do: Enum.concat(v1, v2)

  defp debug(map) do
    Logger.debug(
      "Map parsed: vnum=#{map.vnum} size=#{map.width}x#{map.height} " <>
        "portals=#{length(map.portals)} npcs=#{length(map.npcs)}"
    )
  end
end
