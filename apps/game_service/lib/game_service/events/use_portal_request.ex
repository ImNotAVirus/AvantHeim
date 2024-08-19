defmodule GameService.Events.UsePortalRequest do
  @moduledoc """
  Event triggered when a player stay on a portal.
  """

  use ElvenGard.ECS.Event, fields: [:player_id]

  @type t :: %__MODULE__{player_id: non_neg_integer()}
end
