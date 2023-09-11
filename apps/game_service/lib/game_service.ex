defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias ElvenGard.ECS.Entity

  alias GameService.PlayerEntity
  alias GameService.PlayerComponents.EndpointComponent

  def entity_type(%PlayerEntity{}), do: :character
  def entity_id(%PlayerEntity{id: id}), do: id

  def load_bundle(%Entity{id: {:player, _}} = entity, component) do
    PlayerEntity.load(entity, component)
  end

  def broadcast_to(maybe_events, maybe_endpoints) do
    events = List.wrap(maybe_events)
    endpoints = List.wrap(maybe_endpoints)

    for %EndpointComponent{pid: pid} <- endpoints, event <- events do
      send(pid, event)
    end
  end
end
