defmodule GameService.Events.EntityMessage do
  @moduledoc """
  Event triggered when an Entity send a message.
  """

  use ElvenGard.ECS.Event,
    fields: [
      entity_type: nil,
      entity_id: nil,
      scope: nil,
      # Target is only for the private scope
      target_type: :player,
      target_id: nil,
      message: nil
    ]

  alias ElvenData.Enums.EntityEnums

  @type scope :: :private | :group | :map | :family | :instance | :system

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          scope: scope(),
          target_type: EntityEnums.entity_type_keys(),
          target_id: non_neg_integer(),
          message: String.t()
        }
end
