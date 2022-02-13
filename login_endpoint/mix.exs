defmodule LoginEndpoint.MixProject do
  use Mix.Project

  def project do
    [
      app: :login_endpoint,
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
      mod: {LoginEndpoint.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:elven_core, path: "../elven_core"},
      {:elven_database, path: "../elven_database"},
      # {:session_service, path: "../session_service"},
      {:ranch, "~> 2.0"}
    ]
  end
end
