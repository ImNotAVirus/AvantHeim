# Configure and run tests
ExUnit.configure(exclude: [:deployment])
ExUnit.start(capture_log: true)
