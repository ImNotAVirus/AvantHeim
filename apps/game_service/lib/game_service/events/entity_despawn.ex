defmodule GameService.Events.EntityDespawn do
  @moduledoc """
  Event triggered when a new entity is despawned.

  Contains the entity struct.
  """
  use ElvenGard.ECS.Event, fields: [entity: nil, components: []]

  alias ElvenGard.ECS.Entity

  @type t :: %__MODULE__{entity: Entity.t(), components: [Component.t()]}
end
