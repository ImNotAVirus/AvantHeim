defmodule GameService.Events.EntitySpawned do
  @moduledoc """
  Event triggered when a new entity is spawned.

  Contains the entity struct and his specs.
  """
  use ElvenGard.ECS.Event, fields: [entity: nil, components: [], children: [], parent: nil]

  alias ElvenGard.ECS.{Component, Entity}

  @type t :: %__MODULE__{
          entity: Entity.t(),
          components: [Component.t()],
          children: [Entity.t()],
          parent: Entity.t() | nil
        }
end
