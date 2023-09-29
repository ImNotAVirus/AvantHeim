defmodule ElvenPackets.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_packets,
      version: "0.1.0",
      elixir: "~> 1.13",
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env()),
      elixirc_options: [warnings_as_errors: true]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:elvengard_network, "~> 0.1.1", runtime: false},
      # FIXME: Need to release a new version for the :if serializer 
      {:elvengard_network, github: "elvengard-mmo/elvengard_network", runtime: false},
      {:elven_data, path: "../elven_data", runtime: false},
      {:game_service, path: "../game_service", runtime: false},
      {:simple_enum, "~> 0.1", runtime: false}
    ]
  end
end
