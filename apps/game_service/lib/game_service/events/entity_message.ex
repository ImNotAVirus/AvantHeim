defmodule GameService.Events.EntityMessage do
  @moduledoc """
  Event triggered when an Entity send a message.
  """

  use ElvenGard.ECS.Event,
    fields: [
      entity_type: nil,
      entity_id: nil,
      scope: nil,
      # player_name is only for the private scope
      player_name: nil,
      message: nil
    ]

  alias ElvenData.Enums.EntityEnums

  @type scope :: :private | :group | :map | :family | :instance | :system

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          scope: scope(),
          player_name: String.t(),
          message: String.t()
        }
end
