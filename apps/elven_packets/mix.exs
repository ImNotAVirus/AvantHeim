defmodule ElvenPackets.MixProject do
  use Mix.Project

  def project do
    [
      app: :elven_packets,
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
      {:elvengard_network, github: "imnotavirus/elvengard_network", branch: "documentation"},
      {:simple_enum, "~> 0.1"}
    ]
  end
end
