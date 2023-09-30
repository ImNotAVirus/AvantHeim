defmodule GameService.Events.MapEffect do
  @moduledoc """
  Event triggered when an effect must be broadcast on the map.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id, :effect_id]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          effect_id: non_neg_integer()
        }
end
