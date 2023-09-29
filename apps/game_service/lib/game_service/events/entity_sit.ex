defmodule GameService.Events.EntitySit do
  @moduledoc """
  Event triggered when an Entity sitting down or standing up.
  """

  use ElvenGard.ECS.Event, fields: []

  @type t :: %__MODULE__{}
end
