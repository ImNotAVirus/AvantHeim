defmodule GameService.Events.PlayerDisconnected do
  @moduledoc """
  Event triggered when a player disconnect.

  This event is responsible for cleaning the state and send a notification to clients
  """

  use ElvenGard.ECS.Event, fields: [:account_id]

  @type t :: %__MODULE__{account_id: non_neg_integer()}
end
