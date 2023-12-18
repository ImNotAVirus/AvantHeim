import Config

## Forward telemetry

config :game_service, :admin_service,
  name: AdminService.TelemetryReceiver,
  node: :"admin@#{elem(:inet.gethostname(), 1)}"
