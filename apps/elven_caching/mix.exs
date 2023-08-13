defmodule ElvenCaching.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_caching,
      version: "0.1.0",
      elixir: "~> 1.13",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Specifies which paths to compile per environment.
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
      {:elven_core, path: "../elven_core"},
      {:elven_enums, path: "../elven_enums"},
      {:memento, "~> 0.3"},
      {:ex_unit_clustered_case, "~> 0.4", only: :test}
    ]
  end
end