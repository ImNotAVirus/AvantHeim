import Config

## Login configs

config :login_service,
  client_version: System.get_env("CLIENT_VERSION", "0.9.3.3184"),
  world_ip: "127.0.0.1",
  world_port: 5000
