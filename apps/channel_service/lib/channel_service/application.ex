defmodule ChannelService.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    topologies = Application.get_env(:libcluster, :topologies, [])

    children = [
      {Cluster.Supervisor, [topologies, [name: LoginService.ClusterSupervisor]]},
      {ElvenGard.ECS.MnesiaBackend.ClusterManager, [auto_connect: false]},
      {ElvenCaching.SessionRegistry, [disable_clean: true]},
      {ElvenCaching.CharacterRegistry, []},
      {ChannelService.PresenceManager, []},
      {ChannelService.Endpoint, name: ChannelService.Endpoint}
    ]

    opts = [strategy: :rest_for_one, name: ChannelService.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
