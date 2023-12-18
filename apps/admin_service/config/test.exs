import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :admin_service, AdminServiceWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "f/9TgigmhwvWewHtjsq0vTdmyqOFZJlD4RRAPlIt+mzD0G9n3CPFUYK5h20Qcj+r",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
