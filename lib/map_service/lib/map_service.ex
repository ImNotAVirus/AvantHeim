defmodule MapService do
  @moduledoc """
  Documentation for `MapService`.
  """

  @tree_name Application.compile_env!(:map_service, :tree_name)

  ## Internal API

  def supervisor(base \\ @tree_name), do: :"#{base}.Supervisor"
  def map_registry(base \\ @tree_name), do: :"#{base}.MapRegistry"
  def static_maps_supervisor(base \\ @tree_name), do: :"#{base}.StaticMapsSupervisor"
  def instances_supervisor(base \\ @tree_name), do: :"#{base}.InstancesSupervisor"
  def loader(base \\ @tree_name), do: :"#{base}.MapLoader"
end
