# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

## Login configs

config :login_service,
  client_version: System.get_env("CLIENT_VERSION", "0.9.3.3152"),
  world_ip: "127.0.0.1",
  world_port: 5000

config :login_service, LoginService.Endpoint,
  listener_name: :login_service,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 4002],
  protocol: LoginService.Endpoint.Protocol

config :login_service, LoginService.Endpoint.Protocol,
  packet_handler: LoginService.Endpoint.PacketHandlers,
  network_codec: LoginService.Endpoint.NetworkCodec

# Import global config
import_config "../../../config/config.exs"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("#{__DIR__}/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
