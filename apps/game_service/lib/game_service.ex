defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias ElvenGard.ECS.Entity

  alias GameService.PlayerBundle
  alias GameService.PlayerComponents.EndpointComponent

  def entity_type(%PlayerBundle{}), do: :character
  def entity_id(%PlayerBundle{id: id}), do: id

  def load_bundle(%Entity{id: {:player, _}} = entity, components) do
    PlayerBundle.load(entity, components)
  end

  def broadcast_to(maybe_events, maybe_endpoints) do
    events = List.wrap(maybe_events)
    endpoints = List.wrap(maybe_endpoints)

    for %EndpointComponent{pid: pid} <- endpoints, event <- events do
      send(pid, event)
    end
  end
end
