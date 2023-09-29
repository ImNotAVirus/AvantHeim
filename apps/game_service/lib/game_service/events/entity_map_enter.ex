defmodule GameService.Events.EntityMapEnter do
  @moduledoc """
  Event triggered when a new entity enter on a map.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer()
        }
end
