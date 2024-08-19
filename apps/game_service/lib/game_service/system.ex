defmodule GameService.System do
  @moduledoc """
  TODO: Documentation for GameService.System
  """

  require Logger

  alias ElvenGard.ECS.{Entity, Query}

  alias GameService.EntityComponents.PositionComponent
  alias GameService.PlayerComponents.EndpointComponent

  ## Public API

  @doc false
  defmacro __using__(opts) do
    quote location: :keep do
      use ElvenGard.ECS.System, unquote(opts)

      alias ElvenGard.ECS.{Query, Command}

      alias GameService.EntityComponents, as: E
      alias GameService.PlayerComponents, as: P
      alias GameService.MonsterComponents, as: M
      alias GameService.System
    end
  end

  @spec map_event(event | [event], PositionComponent.t(), [Entity.t()]) :: [event]
        when event: any()
  def map_event(events, %PositionComponent{map_ref: map_ref}, ignore_entities \\ []) do
    GameService.broadcast_to(events, get_endpoints(map_ref, ignore_entities))
  end

  def error(mod, error, event) do
    Logger.error(
      "[#{inspect(mod)}] #{inspect(event.__struct__)} event failed " <>
        "with value #{inspect(error)} - #{inspect(event, limit: :infinity)}"
    )
  end

  ## Helpers

  defp get_endpoints(map_ref, []) do
    EndpointComponent
    |> Query.select(partition: map_ref)
    |> Query.all()
  end

  defp get_endpoints(map_ref, ignore_entities) do
    # Get all Entities with an endpoints on the current map
    entities =
      {ElvenGard.ECS.Entity, EndpointComponent}
      |> Query.select(partition: map_ref)
      |> Query.all()

    # Get Endpoints
    entities
    |> Enum.reject(&(elem(&1, 0) in ignore_entities))
    |> Enum.map(&elem(&1, 1))
  end
end
