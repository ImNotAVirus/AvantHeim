defmodule ElvenGard.ECS.LiveDashboard.MixProject do
  use Mix.Project

  def project do
    [
      app: :elvengard_ecs_live_dashboard,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {ElvenGard.ECS.LiveDashboard, []},
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:phoenix_live_dashboard, "~> 0.8"},
      {:contex, "~> 0.5"}
    ]
  end
end
