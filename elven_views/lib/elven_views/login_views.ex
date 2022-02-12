defmodule ElvenViews.LoginViews do
  @moduledoc """
  TODO: Documentation
  """

  import ElvenViews, only: [optional_param: 2, required_param: 2]

  alias ElvenViews.LoginPackets.NsTeST.Channel
  alias ElvenViews.LoginPackets.{FailcPacket, NsTeSTPacket}

  @behaviour ElvenViews

  ## Public API

  @impl true
  def render(:login_error, args) do
    %FailcPacket{error: optional_param(args, :error)}
  end

  def render(:login_succeed, args) do
    username = required_param(args, :username)
    session_id = required_param(args, :session_id)

    # TODO: Support multiple channels
    ip = required_param(args, :ip)
    port = required_param(args, :port)

    %NsTeSTPacket{
      session_id: session_id,
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
