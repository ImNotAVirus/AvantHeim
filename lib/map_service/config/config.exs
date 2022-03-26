# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

## MapService configs

config :map_service, :loader_config, lazy: false

# Import global config
import_config "../../../config/config.exs"

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("#{__DIR__}/#{config_env()}.exs") do
  import_config "#{config_env()}.exs"
end
