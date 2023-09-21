defmodule GameService.Events.Movement do
  @moduledoc """
  Event triggered when an Entity move.
  """

  use ElvenGard.ECS.Event, fields: []

  @type t :: %__MODULE__{}
end
