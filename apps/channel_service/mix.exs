defmodule ChannelService.MixProject do
  use Mix.Project

  def project do
    [
      app: :channel_service,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
      # elixirc_options: [warnings_as_errors: true]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {ChannelService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elvengard_network, "~> 0.1.1"},
      {:libcluster, "~> 3.3"},
      {:elven_database, path: "../elven_database"},
      {:elven_caching, path: "../elven_caching"},
      {:elven_packets, path: "../elven_packets"},
      {:game_service, path: "../game_service", runtime: false}
    ]
  end
end
