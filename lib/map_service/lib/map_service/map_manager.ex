defmodule MapService.MapManager do
  @moduledoc """
  TODO: Documentation
  """

  use Supervisor

  alias MapService.MapLoader

  @type map_config :: map

  ## Public API

  @spec start_link(Keyword.t()) :: Supervisor.on_start()
  def start_link(opts) do
    name = opts[:name] || raise ArgumentError, "must supply a name"
    Supervisor.start_link(__MODULE__, opts, name: supervisor(name))
  end

  @spec start_static_map(atom, non_neg_integer) :: {:ok, map_config()} | {:error, any}
  def start_static_map(manager, map_vnum) do
    GenServer.call(loader(manager), {:start_static_map, map_vnum})
  end

  ## GenServer behaviour

  @impl true
  def init(opts) do
    name = opts[:name]

    children = [
      {Registry, keys: :unique, name: map_registry(name)},
      {DynamicSupervisor, strategy: :one_for_one, name: static_maps_supervisor(name)},
      {DynamicSupervisor, strategy: :one_for_one, name: instances_supervisor(name)},
      {MapLoader,
       [
         name: loader(name),
         config: Application.get_env(:map_service, :loader_config, []),
         map_registry: map_registry(name),
         static_maps_supervisor: static_maps_supervisor(name),
         instances_supervisor: instances_supervisor(name)
       ]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  ## Private functions

  defp supervisor(base_mod), do: :"#{base_mod}.Supervisor"
  defp map_registry(base_mod), do: :"#{base_mod}.MapRegistry"
  defp static_maps_supervisor(base_mod), do: :"#{base_mod}.StaticMapsSupervisor"
  defp instances_supervisor(base_mod), do: :"#{base_mod}.InstancesSupervisor"
  defp loader(base_mod), do: :"#{base_mod}.MapLoader"
end
