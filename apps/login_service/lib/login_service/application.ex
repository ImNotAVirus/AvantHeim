defmodule LoginService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      {Cluster.Supervisor, [topologies, [name: LoginService.ClusterSupervisor]]},
      {LoginService.SessionManager, []},
      {LoginService.Endpoint, name: LoginService.Endpoint}
    ]

    opts = [strategy: :rest_for_one, name: LoginService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
