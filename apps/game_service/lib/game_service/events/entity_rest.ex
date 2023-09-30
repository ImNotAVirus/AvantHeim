defmodule GameService.Events.EntityRest do
  @moduledoc """
  Event triggered when an Entity sitting down or standing up.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id, :is_sitting]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          is_sitting: boolean()
        }
end
