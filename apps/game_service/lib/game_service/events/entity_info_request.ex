defmodule GameService.Events.EntityInfoRequest do
  @moduledoc """
  Event triggered when a player request informations for an Entity.
  """

  use ElvenGard.ECS.Event,
    fields: [
      :entity_type,
      :entity_id,
      :target_type,
      :target_id
    ]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          target_type: EntityEnums.entity_type_keys(),
          target_id: non_neg_integer()
        }
end
