defmodule MapService.MapSupervisor do
  @moduledoc """
  TODO: Documentation
  """

  use Supervisor

  ## Public API

  @spec start_link(Keyword.t()) :: Supervisor.on_start()
  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, [], name: MapService.supervisor())
  end

  ## GenServer behaviour

  @impl true
  def init(_opts) do
    children = [
      {Registry, keys: :unique, name: MapService.map_registry()},
      {MapService.MapLoader,
       [
         name: MapService.loader(),
         config: Application.get_env(:map_service, :loader_config, []),
         map_registry: MapService.map_registry(),
         static_maps_supervisor: MapService.static_maps_supervisor(),
         instances_supervisor: MapService.instances_supervisor()
       ]},
      {DynamicSupervisor, strategy: :one_for_one, name: MapService.static_maps_supervisor()},
      {DynamicSupervisor, strategy: :one_for_one, name: MapService.instances_supervisor()}
    ]

    Supervisor.init(children, strategy: :rest_for_one)
  end
end
