defmodule ElvenViews.LoginViews do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenViews

  alias ElvenViews.LoginPackets.NsTeST.Channel
  alias ElvenViews.LoginPackets.{FailcPacket, NsTeSTPacket}

  ## Public API

  @impl true
  def render(:failc, args) do
    %FailcPacket{error: optional_param(args, :error)}
  end

  def render(:nstest, args) do
    encryption_key = required_param(args, :encryption_key)
    username = required_param(args, :username)

    # TODO: Support multiple channels
    ip = required_param(args, :ip)
    port = required_param(args, :port)

    %NsTeSTPacket{
      encryption_key: encryption_key,
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
