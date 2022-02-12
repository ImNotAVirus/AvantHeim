defmodule ElvenViews.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_views,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:core, path: "../core"},
      {:database_service, path: "../database_service"},
      {:simple_enum, "~> 0.1"}
    ]
  end
end
