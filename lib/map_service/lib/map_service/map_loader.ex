defmodule MapService.MapLoader do
  @moduledoc """
  TODO: Documentation
  """

  use GenServer

  require Logger

  alias MapService.ConfigFile.MapConfig
  alias MapService.MapProcess

  @root_path :code.priv_dir(:map_service)

  @default_config [
    lazy: true,
    base_maps_path: Path.join(@root_path, "base_maps")
  ]

  ## Delegates

  ## Public API

  @spec start_link(Keyword.t()) :: GenServer.on_start()
  def start_link(opts) do
    name = require_opt(opts, :name)
    static_maps_sup = require_opt(opts, :static_maps_supervisor)
    instances_sup = require_opt(opts, :instances_supervisor)
    map_registry = require_opt(opts, :map_registry)

    args_config = Keyword.get(opts, :config, [])
    config = Keyword.merge(@default_config, args_config)

    check_config!(name, config)

    state =
      config
      |> Enum.into(%{})
      |> Map.merge(%{
        name: name,
        static_maps_sup: static_maps_sup,
        instances_sup: instances_sup,
        map_registry: map_registry
      })

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
  def handle_continue(:init_base_maps, %{base_maps_path: base_maps_path, name: name} = state) do
    map_dirs = Path.wildcard("#{base_maps_path}/*")
    map_vnums = Enum.map(map_dirs, &(&1 |> Path.basename() |> String.to_integer()))

    Enum.each(map_vnums, &start_static_map(&1, state))

    Logger.info("#{inspect(name)} is now monitoring #{length(map_vnums)} maps")

    {:noreply, state}
  end

  @impl true
  def handle_call({:start_static_map, map_vnum}, _from, state) do
    {:reply, start_static_map(map_vnum, state), state}
  end

  ## Private functions

  defp static_map(vnum), do: {:static, vnum}

  defp static_via_registry(map_registry, vnum) do
    {:via, Registry, {map_registry, static_map(vnum)}}
  end

  defp require_opt(opts, name) do
    opts[name] || raise ArgumentError, "must supply a #{name}"
  end

  defp check_config!(name, config) do
    valid_keys = Keyword.keys(@default_config)

    Enum.each(config, fn {key, value} ->
      if key not in valid_keys,
        do: raise(ArgumentError, "unknown config #{key}: #{value} for #{inspect(name)}")
    end)
  end

  defp start_static_map(vnum, state) do
    %{map_registry: map_registry, static_maps_sup: static_maps_sup} = state

    with {:lookup, []} <- {:lookup, Registry.lookup(map_registry, static_map(vnum))},
         {:config, {:ok, config}} <- {:config, get_map_config(vnum, state)} do
      args = {config, name: static_via_registry(map_registry, vnum)}

      {:ok, _process} = DynamicSupervisor.start_child(static_maps_sup, {MapProcess, args})
      {:ok, config}
    else
      {:lookup, [_]} -> {:error, :already_registered}
      {:config, error} -> error
    end
  end

  defp get_map_config(vnum, %{base_maps_path: base_maps_path}) do
    dir = Path.join(base_maps_path, Integer.to_string(vnum))

    if not File.dir?(dir) do
      {:error, :enotdir}
    else
      Logger.debug("Parsing map##{vnum}: #{Path.relative_to_cwd(dir)}")

      "#{dir}/*.{yml,yaml}"
      |> Path.wildcard()
      |> Enum.map(&YamlElixir.read_from_file!(&1, atoms: true))
      |> Enum.reduce(%{}, fn element, acc -> Map.merge(acc, element, &merger/3) end)
      |> Map.put("map_id", vnum)
      |> Map.put("map_dir", dir)
      |> MapConfig.new()
      |> tap(&debug/1)
      |> then(&{:ok, &1})
    end
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
