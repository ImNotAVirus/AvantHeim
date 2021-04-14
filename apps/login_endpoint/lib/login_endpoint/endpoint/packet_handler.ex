defmodule LoginEndpoint.Endpoint.PacketHandler do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias Core.Socket

  ## Public API

  @spec handle_packet(String.t(), map, map) :: any
  def handle_packet("NoS0575", args, %Socket{} = socket) do
    with {:ok, _client_version} <- check_client_version(args, socket),
         {:ok, _client_checksum} <- check_client_checksum(args, socket),
         {:ok, _guid} <- check_guid(args, socket),
         {:ok, {_username, _password}} <- check_credentials(args, socket) do
    else
      {:error, error} -> Logger.warn(error)
    end

    IO.inspect(args)
    IO.inspect(socket)
    :ok
  end

  ## Private functions

  defp check_client_version(%{client_version: version}, socket) do
    requirement = Application.fetch_env!(:login_endpoint, :client_version)
    err = "Invalid client version for #{socket.id} (got: #{version}; expected: #{requirement})"

    case version do
      ^requirement -> {:ok, version}
      _ -> {:error, err}
    end
  end

  defp check_client_checksum(%{client_checksum: client_checksum}, _socket) do
    # Currently unused
    # TODO: Can be checked later for client modification for example
    {:ok, client_checksum}
  end

  defp check_guid(%{guid: guid}, socket) do
    # Currently unused
    # TODO: Can be saved later in database to check multiclients/multiaccounts for example
    Logger.debug("GUID for #{socket.id}: #{guid}")
    {:ok, guid}
  end

  defp check_credentials(%{username: username, password: password}, socket) do
    # Currently unused
    # TODO: Check in database later
    expected_pass =
      "C7AD44CBAD762A5DA0A452F9E854FDC1E0E7A52A38015F23F3EAB1D80B931DD472634DFAC71CD34EBC35D16AB7FB8A90C81F975113D6C7538DC69DD8DE9077EC"

    case {username, password} do
      {"admin", ^expected_pass} = credentials -> {:ok, credentials}
      _ -> {:error, "Invalid credentials for #{socket.id} (username: #{username})"}
    end
  end
end
