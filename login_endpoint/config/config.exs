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

## Database configs

config :elven_database, ecto_repos: [ElvenDatabase.Repo]

config :elven_database, ElvenDatabase.Repo,
  database: "elvengard_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  port: 5432
