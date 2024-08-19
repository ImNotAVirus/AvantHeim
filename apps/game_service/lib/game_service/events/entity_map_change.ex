defmodule GameService.Events.EntityMapChange do
  @moduledoc """
  Event triggered when an entity change map.
  """

  use ElvenGard.ECS.Event,
    fields: [
      :entity_type,
      :entity_id,
      :destination_map_id,
      :destination_map_ref,
      :destination_map_x,
      :destination_map_y
    ]

  alias ElvenData.Enums.EntityEnums
  alias GameService.EntityComponents.PositionComponent

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          destination_map_id: PositionComponent.map_id(),
          destination_map_ref: PositionComponent.map_ref(),
          destination_map_x: PositionComponent.map_x(),
          destination_map_y: PositionComponent.map_y()
        }
end
