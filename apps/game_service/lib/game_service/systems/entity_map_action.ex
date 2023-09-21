defmodule GameService.EntityMapActionSystem do
  @moduledoc """
  TODO: Documentation for GameService.EntityMapActionSystem

  Note: All Entities must have a PositionComponent or this system will raise
  """

  use ElvenGard.ECS.System,
    lock_components: [
      GameService.EntityComponents.PositionComponent,
      GameService.EntityComponents.DirectionComponent,
      GameService.EntityComponents.LevelComponent,
      GameService.PlayerComponents.HeroLevelComponent,
      GameService.EntityComponents.CombatComponent,
      # GameService.EntityComponents.BuffComponent,
      GameService.EntityComponents.SpeedComponent,
      GameService.EntityComponents.SittingComponent
    ],
    event_subscriptions: [
      GameService.Events.DirectionChanged,
      GameService.Events.EntityInfoRequest,
      GameService.Events.Movement,
      GameService.Events.Sitting
    ]

  require Logger

  # System behaviour

  @impl true
  def run(event, _delta) do
    Logger.warn("#{inspect(__MODULE__)} unhandled event #{inspect(event)}")
  end
end
