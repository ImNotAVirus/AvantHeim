defmodule GameService.Events.EntityChangeDirection do
  @moduledoc """
  Event triggered when an Entity is writting in the chat.
  """

  use ElvenGard.ECS.Event, fields: [:entity_type, :entity_id, :message]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          message: String.t()
        }
end
