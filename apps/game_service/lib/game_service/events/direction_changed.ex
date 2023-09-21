defmodule GameService.Events.DirectionChanged do
  @moduledoc """
  Event triggered when an Entity change his direction.
  """
  use ElvenGard.ECS.Event, fields: []

  @type t :: %__MODULE__{}
end
