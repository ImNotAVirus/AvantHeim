defmodule GameService.Events.EntityChangeDirection do
  @moduledoc """
  Event triggered when an Entity change his direction.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id, :value]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          value: EntityEnums.direction_type_keys()
        }
end
