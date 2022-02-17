# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

## General configs

config :logger, :console,
  level: :debug,
  format: "[$time] [$level] $metadata$message\n",
  metadata: [:application, :socket_id],
  colors: [info: :green]

## Channel configs

config :channel_service,
  packet_schemas: ChannelService.PacketSchemas

config :channel_service, ChannelService.Endpoint,
  listener_name: :channel_service,
  transport: :ranch_tcp,
  transport_opts: [ip: {127, 0, 0, 1}, port: 5000],
  protocol: ChannelService.Endpoint.Protocol,
  protocol_opts: []

## Database configs

config :elven_database, ecto_repos: [ElvenDatabase.Repo]

config :elven_database, ElvenDatabase.Repo,
  database: "elvengard_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  port: 5432

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("config/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
