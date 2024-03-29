defmodule GameService do
  @moduledoc """
  Documentation for `GameService`.
  """

  alias ElvenGard.ECS.Entity

  alias GameService.{MonsterBundle, PlayerBundle}
  alias GameService.PlayerComponents.EndpointComponent

  def entity_type(%PlayerBundle{}), do: :player
  def entity_type(%MonsterBundle{}), do: :monster
  def entity_type(%Entity{id: {type, _}}), do: type

  def entity_id(%PlayerBundle{id: id}), do: id
  def entity_id(%MonsterBundle{id: id}), do: id
  def entity_id(%Entity{id: {_, id}}), do: id

  def real_entity_id(entity_type, entity_id), do: {entity_type, entity_id}

  def load_bundle(%Entity{id: {type, _}} = entity, components) do
    case type do
      :player -> PlayerBundle.load(entity, components)
      :monster -> MonsterBundle.load(entity, components)
    end
  end

  def preload_bundle(%Entity{id: {type, _}} = entity, components) do
    case type do
      :player -> PlayerBundle.preload(entity, components)
      :monster -> MonsterBundle.preload(entity, components)
    end
  end

  def send_to(maybe_events, %EndpointComponent{pid: pid}) do
    events = List.wrap(maybe_events)

    for event <- events do
      send(pid, event)
    end
  end

  def broadcast_to(maybe_events, maybe_endpoints) do
    events = List.wrap(maybe_events)
    endpoints = List.wrap(maybe_endpoints)

    for %EndpointComponent{pid: pid} <- endpoints, event <- events do
      send(pid, event)
    end
  end
end
