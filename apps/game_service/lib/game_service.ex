defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias ElvenGard.ECS.Entity

  alias GameService.PlayerBundle
  alias GameService.PlayerComponents.EndpointComponent

  def entity_type(%PlayerBundle{}), do: :player
  def entity_type(%Entity{id: {type, _}}), do: type

  def entity_id(%PlayerBundle{id: id}), do: id
  def entity_id(%Entity{id: {_, id}}), do: id

  def real_entity_id(entity_type, entity_id), do: {entity_type, entity_id}

  def load_bundle(%Entity{id: {:player, _}} = entity, components) do
    PlayerBundle.load(entity, components)
  end

  def preload_bundle(%Entity{id: {:player, _}} = entity, components) do
    PlayerBundle.preload(entity, components)
  end

  def broadcast_to(maybe_events, maybe_endpoints) do
    events = List.wrap(maybe_events)
    endpoints = List.wrap(maybe_endpoints)

    for %EndpointComponent{pid: pid} <- endpoints, event <- events do
      send(pid, event)
    end
  end
end
