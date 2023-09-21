defmodule GameService.Events.EntityMove do
  @moduledoc """
  Event triggered when an Entity move.
  """

  use ElvenGard.ECS.Event, fields: []

  @type t :: %__MODULE__{}
end
