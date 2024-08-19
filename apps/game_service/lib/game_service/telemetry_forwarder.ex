defmodule GameService.TelemetryForwarder do
  @moduledoc """
  TODO: Documentation for GameService.TelemetryForwarder
  """

  ## Public API

  def setup() do
    _events =
      :telemetry.attach_many(
        "elvengard_ecs-telemetry-handler",
        events(),
        &__MODULE__.handle_event/4,
        nil
      )
  end

  ## Internal API

  @doc false
  def handle_event(event, measurements, metadata, _config) do
    :rpc.call(admin_service(), :telemetry, :execute, [event, measurements, metadata])
  end

  ## Private API

  defp events() do
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

  defp admin_service() do
    _admin_service = Application.fetch_env!(:game_service, :admin_service)
  end
end
