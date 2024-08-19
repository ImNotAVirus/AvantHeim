import Config

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
if File.exists?("#{__DIR__}/runtime.#{config_env()}.exs") do
  Code.require_file("runtime.#{config_env()}.exs", __DIR__)
end
