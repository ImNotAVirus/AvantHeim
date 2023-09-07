defmodule ElvenPackets.Views.LoginViews do
  @moduledoc """
  TODO: ElvenPackets.Views.LoginViews
  """

  use ElvenGard.Network.View

  import ElvenPackets.View, only: [optional_param: 2, required_param: 2]

  alias ElvenPackets.Server.LoginPackets.{Failc, NsTeST}
  alias ElvenPackets.SubPackets.Login.NsTeST.Channel

  ## Public API

  # FIXME: Move to ElvenGard.Network.View
  def render(name), do: render(name, [])

  @impl true
  def render(:failc, args) do
    %Failc{error: optional_param(args, :error)}
  end

  def render(:nstest, args) do
    encryption_key = required_param(args, :encryption_key)
    region = required_param(args, :region)
    username = required_param(args, :username)

    # TODO: Support multiple channels
    ip = required_param(args, :ip)
    port = required_param(args, :port)

    %NsTeST{
      encryption_key: encryption_key,
      region: region,
      username: username,
      server_list: [
        %Channel{
          id: 1,
          world_id: 1,
          world_name: "ElvenGard",
          ip: ip,
          port: port,
          population: 4
        }
      ]
    }
  end
end
