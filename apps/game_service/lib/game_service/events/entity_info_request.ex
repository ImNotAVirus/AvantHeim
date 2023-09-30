defmodule GameService.Events.EntityInfoRequest do
  @moduledoc """
  Event triggered when a player request informations for an Entity.
  """

  use ElvenGard.ECS.Event,
    fields: [
      :entity_type,
      :entity_id,
      :level,
      :hero_level,
      :hp,
      :hp_max,
      :mp,
      :mp_max,
      :buffs
    ]

  alias ElvenData.Enums.EntityEnums

  @type t :: %__MODULE__{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: non_neg_integer(),
          level: non_neg_integer(),
          hero_level: non_neg_integer(),
          hp: non_neg_integer(),
          hp_max: non_neg_integer(),
          mp: non_neg_integer(),
          mp_max: non_neg_integer(),
          buffs: list()
        }
end
