defmodule ElvenGard.ECS.LiveDashboard do
  @moduledoc """
  Documentation for `ElvenGard.ECS.LiveDashboard`.
  """

  use Application

  ## Public API

  def start(_, _) do
    :ok =
      :telemetry.attach_many(
        "elvengard_ecs-telemetry-handler",
        telemetry_events(),
        &ElvenGard.ECS.LiveDashboard.Handler.handle_event/4,
        nil
      )

    children = [
      # ElvenGard.ECS.LiveDashboard.Store
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  ## Private functions

  defp telemetry_events do
    [
      [:elvengard_ecs, :partition_init],
      [:elvengard_ecs, :startup_system_run, :start],
      [:elvengard_ecs, :startup_system_run, :stop],
      [:elvengard_ecs, :startup_system_run, :exception],
      [:elvengard_ecs, :system_run, :start],
      [:elvengard_ecs, :system_run, :stop],
      [:elvengard_ecs, :system_run, :exception]
    ]
  end
end
