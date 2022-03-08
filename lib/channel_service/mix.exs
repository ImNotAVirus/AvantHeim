defmodule ChannelService.MixProject do
  use Mix.Project

  def project do
    [
      app: :channel_service,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
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
      {:elven_core, path: "../elven_core"},
      {:elven_database, path: "../elven_database"},
      {:elven_views, path: "../elven_views"},
      {:elven_caching, path: "../elven_caching"},
      {:map_service, path: "../map_service"},
      {:libcluster, "~> 3.3"},
      {:ranch, "~> 2.0"}
    ]
  end
end
