defmodule GameService.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_service,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
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
      extra_applications: [:logger],
      mod: {GameService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elvengard_cluster, github: "elvengard-mmo/elvengard_cluster", optional: true},
      {:elvengard_ecs, github: "elvengard-mmo/elvengard_ecs"},
      {:libcluster, "~> 3.3", optional: true},
      {:elven_data, path: "../elven_data"}
    ]
  end
end
