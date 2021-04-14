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
  format: "$time $metadata[$level] $message\n",
  metadata: [:application],
  colors: [info: :green]

## Login configs

config :login_endpoint,
  client_version: System.get_env("CLIENT_VERSION", "0.9.8.3115")

config :login_endpoint, LoginEndpoint.Endpoint,
  listener_name: :login_endpoint,
  transport: :ranch_tcp,
  transport_opts: [ip: {127, 0, 0, 1}, port: 4002],
  protocol: LoginEndpoint.Endpoint.Protocol,
  protocol_opts: []
