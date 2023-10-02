defmodule LoginService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    # FIXME: Hardcoded, move to config
    {:ok, hostname} = :inet.gethostname()
    mnesia_cluster_opts = [auto_connect: true, master: :"channel@#{hostname}"]

    children = [
      {Cluster.Supervisor, [topologies, [name: LoginService.ClusterSupervisor]]},
      {ElvenGard.Cluster.MnesiaClusterManager, mnesia_cluster_opts},
      {LoginService.SessionManager, []},
      {LoginService.Endpoint, name: LoginService.Endpoint}
    ]

    opts = [strategy: :rest_for_one, name: LoginService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
