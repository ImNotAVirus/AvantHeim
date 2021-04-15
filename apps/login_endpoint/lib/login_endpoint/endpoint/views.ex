defmodule LoginEndpoint.Endpoint.Views do
  @moduledoc """
  TODO: Documentation
  """

  ## Public API

  @spec render(atom, map) :: String.t()
  def render(:login_error, %{error: :old_client}), do: "failc 1"
  def render(:login_error, %{error: :already_connected}), do: "failc 4"
  def render(:login_error, %{error: :bad_credentials}), do: "failc 5"
  def render(:login_error, _), do: "failc 2"

  def render(:login_succeed, %{username: username, session_id: session_id}) do
    ip = Application.fetch_env!(:login_endpoint, :world_ip)
    port = Application.fetch_env!(:login_endpoint, :world_port)

    # TODO: get from ervice
    server_list = "#{ip}:#{port}:4:1.1.ElvenGard"
    "NsTeST #{session_id} #{username} #{server_list} -1:-1:-1:10000.10000.1"
  end
end
