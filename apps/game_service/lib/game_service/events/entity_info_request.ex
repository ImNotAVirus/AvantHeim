defmodule GameService.Events.EntityInfoRequest do
  @moduledoc """
  Event triggered when a player request informations for an Entity.
  """

  use ElvenGard.ECS.Event, fields: []

  @type t :: %__MODULE__{}
end
