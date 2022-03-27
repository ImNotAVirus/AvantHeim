defmodule MapService do
  @moduledoc """
  Documentation for `MapService`.
  """

  ## Internal API

  def supervisor(base), do: :"#{base}.Supervisor"
  def map_registry(base), do: :"#{base}.MapRegistry"
  def static_maps_supervisor(base), do: :"#{base}.StaticMapsSupervisor"
  def instances_supervisor(base), do: :"#{base}.InstancesSupervisor"
  def loader(base), do: :"#{base}.MapLoader"
end
