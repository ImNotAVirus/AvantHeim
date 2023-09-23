defmodule GameService.System do
  @moduledoc """
  TODO: Documentation for GameService.System
  """

  alias ElvenGard.ECS.{Entity, Query}

  alias GameService.EntityComponents.PositionComponent
  alias GameService.PlayerComponents.EndpointComponent

  ## Public API

  @spec map_event(any(), PositionComponent.t(), [Entity.t()]) :: any()
  def map_event(event, %PositionComponent{map_ref: map_ref}, ignore_entities \\ []) do
    # Broadcast the entity spawn to players
    GameService.broadcast_to(event, get_endpoints(map_ref, ignore_entities))
  end

  ## Helpers

  defp get_endpoints(map_ref, []) do
    EndpointComponent
    |> Query.select(with: [{PositionComponent, [{:==, :map_ref, map_ref}]}])
    |> Query.all()
  end

  defp get_endpoints(map_ref, ignore_entities) do
    # Get all Entities with an endpoints on the current map
    entities =
      Query.select(
        # TODO: Later support the syntax {ElvenGard.ECS.Entity, EndpointComponent}
        ElvenGard.ECS.Entity,
        with: [{PositionComponent, [{:==, :map_ref, map_ref}]}],
        preload: [EndpointComponent]
      )
      |> Query.all()

    # Get Endpoints
    entities
    |> Enum.reject(&(elem(&1, 0) in ignore_entities))
    |> Enum.map(fn {_entity, components} ->
      Enum.find(components, &(&1.__struct__ == EndpointComponent))
    end)
  end
end
