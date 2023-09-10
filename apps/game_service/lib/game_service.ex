defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias GameService.PlayerEntity

  def entity_type(%PlayerEntity{}), do: :character
  def entity_id(%PlayerEntity{id: id}), do: id

  def broadcast_to(maybe_events, maybe_pids) do
    events = List.wrap(maybe_events)
    pids = List.wrap(maybe_pids)

    for pid <- pids, event <- events do
      send(pid, event)
    end
  end
end
