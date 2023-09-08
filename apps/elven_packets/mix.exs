defmodule ElvenPackets.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_packets,
      version: "0.1.0",
      elixir: "~> 1.13",
      deps: deps(),
      elixirc_paths: elixirc_paths(Mix.env())
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
      {:elvengard_network, "~> 0.1.1"},
      {:elven_i18n, path: "../elven_i18n"},
      {:elven_enums, path: "../elven_enums"},
      {:elven_caching, path: "../elven_caching"},
      {:simple_enum, "~> 0.1"}
    ]
  end
end
