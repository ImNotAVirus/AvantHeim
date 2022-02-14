defmodule LoginService.MixProject do
  use Mix.Project

  def project do
    [
      app: :login_service,
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
      mod: {LoginService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elven_core, path: "../elven_core"},
      {:elven_database, path: "../elven_database"},
      {:elven_views, path: "../elven_views"},
      {:caching_service, path: "../caching_service", runtime: false},
      {:ranch, "~> 2.0"}
    ]
  end
end
