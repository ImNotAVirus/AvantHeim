defmodule MapService.MapSupervisor do
  @moduledoc """
  TODO: Documentation
  """

  use Supervisor

  ## Public API

  @spec start_link(Keyword.t()) :: Supervisor.on_start()
  def start_link(opts) do
    name = opts[:base_name] || raise "must define a base name"
    Supervisor.start_link(__MODULE__, opts, name: MapService.supervisor(name))
  end

  ## GenServer behaviour

  @impl true
  def init(opts) do
    name = opts[:base_name]

    children = [
      {Registry, keys: :unique, name: MapService.map_registry(name)},
      {MapService.MapLoader,
       [
         name: MapService.loader(name),
         config: Application.get_env(:map_service, :loader_config, []),
         map_registry: MapService.map_registry(name),
         static_maps_supervisor: MapService.static_maps_supervisor(name),
         instances_supervisor: MapService.instances_supervisor(name)
       ]},
      {DynamicSupervisor, strategy: :one_for_one, name: MapService.static_maps_supervisor(name)},
      {DynamicSupervisor, strategy: :one_for_one, name: MapService.instances_supervisor(name)}
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end
end
