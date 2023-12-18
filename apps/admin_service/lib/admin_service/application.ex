defmodule AdminService.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      {Cluster.Supervisor, [topologies, [name: AdminService.ClusterSupervisor]]},
      AdminServiceWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:admin_service, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: AdminService.PubSub},
      # Start a worker by calling: AdminService.Worker.start_link(arg)
      # {AdminService.Worker, arg},
      # Start to serve requests, typically the last entry
      AdminServiceWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: AdminService.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    AdminServiceWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
