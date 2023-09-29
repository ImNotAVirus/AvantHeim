defmodule GameService.Events.EntityMove do
  @moduledoc """
  Event triggered when an Entity move.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id, :pos_x, :pos_y, :speed, :checksum]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          pos_x: non_neg_integer(),
          pos_y: non_neg_integer(),
          speed: 0..59,
          checksum: 0 | 1
        }
end
