defmodule GameService.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_service,
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
      mod: {GameService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elvengard_ecs, path: "../../../elvengard_ecs"},
      {:libcluster, "~> 3.3", optional: true}
    ]
  end
end
