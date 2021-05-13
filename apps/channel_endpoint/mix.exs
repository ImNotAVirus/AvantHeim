defmodule ChannelEndpoint.MixProject do
  use Mix.Project

  def project do
    [
      app: :channel_endpoint,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {ChannelEndpoint.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:core, in_umbrella: true},
      {:database_service, in_umbrella: true},
      {:session_service, in_umbrella: true},
      {:ranch, "~> 2.0"}
    ]
  end
end
