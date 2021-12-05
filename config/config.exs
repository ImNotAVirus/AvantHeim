# This file is responsible for configuring your umbrella
# and **all applications** and their dependencies with the
# help of the Config module.
#
# Note that all applications in your umbrella share the
# same configuration and dependencies, which is why they
# all use the same configuration file. If you want different
# configurations or dependencies per app, it is best to
# move said applications out of the umbrella.
import Config

## General configs

config :logger, :console,
  level: :debug,
  format: "[$time] [$level] $metadata$message\n",
  metadata: [:application, :socket_id],
  colors: [info: :green]

config :logger,
  compile_time_purge_matching: [
    [module: ChannelEndpoint.MapManager, function: "parse_map_file/1"]
  ]

## Login configs

config :login_endpoint,
  packet_schemas: LoginEndpoint.PacketSchemas,
  client_version: System.get_env("CLIENT_VERSION", "0.9.3.3152"),
  world_ip: "127.0.0.1",
  world_port: 5000

config :login_endpoint, LoginEndpoint.Endpoint,
  listener_name: :login_endpoint,
  transport: :ranch_tcp,
  transport_opts: [ip: {127, 0, 0, 1}, port: 4002],
  protocol: LoginEndpoint.Endpoint.Protocol,
  protocol_opts: []

## Channel configs

config :channel_endpoint,
  packet_schemas: ChannelEndpoint.PacketSchemas

config :channel_endpoint, ChannelEndpoint.Endpoint,
  listener_name: :channel_endpoint,
  transport: :ranch_tcp,
  transport_opts: [ip: {127, 0, 0, 1}, port: 5000],
  protocol: ChannelEndpoint.Endpoint.Protocol,
  protocol_opts: []

## Database configs

config :database_service, ecto_repos: [DatabaseService.Repo]

config :database_service, DatabaseService.Repo,
  database: "elvengard_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"

## Caching configs

config :mnesia,
  dir: '.mnesia/#{Mix.env()}/#{node()}'
