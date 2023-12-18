defmodule ElvenGard.ECS.LiveDashboard.Handler do
  @moduledoc false

  alias ElvenGard.ECS.LiveDashboard.Store

  require Logger

  ## Public API

  # [:elvengard_ecs, :partition_init]
  # [:elvengard_ecs, :startup_system_run, :start],
  # [:elvengard_ecs, :startup_system_run, :stop],
  # [:elvengard_ecs, :startup_system_run, :exception],
  # [:elvengard_ecs, :system_run, :start],
  # [:elvengard_ecs, :system_run, :stop],
  # [:elvengard_ecs, :system_run, :exception]

  def handle_event([:elvengard_ecs, :partition_init], measurements, metadata, _config) do
    measurements |> IO.inspect()

    info = %{
      id: metadata.id,
      startup_systems: metadata.startup_systems,
      duration: System.convert_time_unit(measurements.duration, :native, :millisecond)
    }

    Logger.info("[#{info.id}] took #{info.duration} ms to start")
    # Store.partition_init(info)
  end

  def handle_event([:elvengard_ecs, :startup_system_run, action], measurements, metadata, _config) do
    IO.inspect(action)
    IO.inspect(metadata)

    info = %{
      partition: metadata.partition,
      system: metadata.system
    }

    Logger.info(
      "[#{info.partition}] #{info.system} #{action} #{inspect(measurements)} - #{inspect(metadata)}"
    )

    # Store.startup_system_run(info)
  end

  def handle_event([:elvengard_ecs, :system_run, action], measurements, metadata, _config) do
    info = %{
      partition: metadata.partition,
      system: metadata.system,
      event: metadata.event
    }

    Logger.info(
      "[#{info.partition}] #{info.system} #{info.event} #{action} #{inspect(measurements)} - #{inspect(metadata)}"
    )

    # Store.system_run(info)
  end
end
