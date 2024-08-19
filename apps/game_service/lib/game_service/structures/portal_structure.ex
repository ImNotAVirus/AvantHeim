defmodule GameService.Structures.PortalStructure do
  @moduledoc """
  Hold a portal info
  """

  alias __MODULE__
  alias GameService.EntityComponents.PositionComponent

  @enforce_keys [
    :source_map_id,
    :source_map_x,
    :source_map_y,
    :destination_map_id,
    :destination_map_ref,
    :destination_map_x,
    :destination_map_y,
    :type
  ]
  defstruct @enforce_keys

  @type t :: %PortalStructure{
          source_map_id: non_neg_integer(),
          source_map_x: non_neg_integer(),
          source_map_y: non_neg_integer(),
          destination_map_id: PositionComponent.map_id(),
          destination_map_ref: PositionComponent.map_ref(),
          destination_map_x: PositionComponent.map_x(),
          destination_map_y: PositionComponent.map_y(),
          type: integer()
        }
end
