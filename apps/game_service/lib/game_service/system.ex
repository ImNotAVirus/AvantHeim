defmodule GameService.System do
  @moduledoc """
  TODO: Documentation for GameService.System
  """

  alias ElvenGard.ECS.Query

  alias GameService.EntityComponents.PositionComponent
  alias GameService.PlayerComponents.EndpointComponent

  @spec map_event(any(), PositionComponent.t()) :: any()
  def map_event(event, %PositionComponent{map_ref: map_ref}) do
    # Get all endpoints on the current map
    endpoints =
      Query.select(
        EndpointComponent,
        with: [{PositionComponent, [{:==, :map_ref, map_ref}]}]
      )
      |> Query.all()

    # Broadcast the entity spawn to players
    GameService.broadcast_to(event, endpoints)
  end
end
