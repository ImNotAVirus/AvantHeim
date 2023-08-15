defmodule PresenceService.MixProject do
  use Mix.Project

  def project do
    [
      app: :presence_service,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {PresenceService.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elven_pubsub, path: "../elven_pubsub"}
    ]
  end
end