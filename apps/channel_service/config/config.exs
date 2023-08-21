# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

## Channel configs

config :channel_service, ChannelService.Endpoint,
  listener_name: :channel_service,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 5000],
  protocol: ChannelService.Endpoint.Protocol

config :channel_service, ChannelService.Endpoint.Protocol,
  packet_handler: ChannelService.Endpoint.PacketHandler,
  network_codec: ChannelService.Endpoint.NetworkCodec

# Import global config
import_config "../../../config/config.exs"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("#{__DIR__}/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
