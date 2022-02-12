defmodule LoginEndpoint.Endpoint.Views do
  @moduledoc """
  TODO: Documentation
  """

  alias LoginEndpoint.Endpoint.NsTeST.Channel
  alias LoginEndpoint.Endpoint.{FailcPacket, NsTeSTPacket}

  ## Public API

  @spec render(atom, map) :: any
  def render(:login_error, %{error: error}), do: %FailcPacket{error: error}
  def render(:login_error, _), do: %FailcPacket{}

  def render(:login_succeed, %{username: username, session_id: session_id}) do
    ip = Application.fetch_env!(:login_endpoint, :world_ip)
    port = Application.fetch_env!(:login_endpoint, :world_port)

    %NsTeSTPacket{
      session_id: session_id,
      username: username,
      # TODO: get from a service
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
