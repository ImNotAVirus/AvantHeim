defmodule CachingService.MixProject do
  use Mix.Project

  def project do
    [
      app: :caching_service,
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
      mod: {CachingService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elven_core, path: "../elven_core"},
      {:elven_enums, path: "../elven_enums"},
      {:memento, "~> 0.3"}
    ]
  end
end