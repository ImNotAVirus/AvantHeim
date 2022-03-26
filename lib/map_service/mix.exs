defmodule MapService.MixProject do
  use Mix.Project

  def project do
    [
      app: :map_service,
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
      mod: {MapService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elven_enums, path: "../elven_enums"},
      {:elven_caching, path: "../elven_caching"},
      {:yaml_elixir, "~> 2.8", runtime: false},
      {:libcluster, "~> 3.3"}
    ]
  end
end
