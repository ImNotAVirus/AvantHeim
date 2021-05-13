defmodule LoginEndpoint.Endpoint.AuthActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias Core.Socket
  alias DatabaseService.Players.{Account, Accounts}
  alias LoginEndpoint.Endpoint.Views

  ## Public API

  @spec login(String.t(), map, map) :: any
  def login("NoS0575", args, %Socket{id: socket_id} = socket) do
    render =
      with {:ok, _client_version} <- check_client_version(args, socket),
           {:ok, _client_checksum} <- check_client_checksum(args, socket),
           {:ok, _guid} <- check_guid(args, socket),
           {:ok, account} <- check_credentials(args, socket),
           {:ok, session_id} <- create_session(args, socket) do
        Logger.debug("Authentication succeed for #{socket_id} (username: #{args.username})")
        Views.render(:login_succeed, %{username: account.username, session_id: session_id})
      else
        {:error, :client_version} ->
          Logger.warn("Invalid client version (got: #{args.version})", socket_id: socket_id)
          Views.render(:login_error, %{error: :old_client})

        {:error, :client_checksum} ->
          Logger.warn("Invalid client checksum (got: #{args.version})", socket_id: socket_id)
          Views.render(:login_error, %{error: :old_client})

        {:error, :bad_credentials} ->
          Logger.warn("Invalid credentials (username: #{args.username})", socket_id: socket_id)
          Views.render(:login_error, %{error: :bad_credentials})

        {:error, :already_connected} ->
          Logger.warn("Already connected (username: #{args.username})", socket_id: socket_id)
          Views.render(:login_error, %{error: :already_connected})

        e ->
          Logger.warn("Got unknown login error: #{inspect(e)}", socket_id: socket_id)
          Views.render(:login_error, %{})
      end

    Socket.send(socket, render)
  end

  ## Private functions

  defp check_client_version(%{client_version: version}, _socket) do
    requirement = Application.fetch_env!(:login_endpoint, :client_version)

    case version do
      ^requirement -> {:ok, version}
      _ -> {:error, :client_version}
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
    # Or if user is banned
    Logger.debug("GUID for #{socket.id}: #{guid}")
    {:ok, guid}
  end

  defp check_credentials(%{username: username, password: password}, _socket) do
    case Accounts.log_in(username, password) do
      %Account{} = account -> {:ok, account}
      nil -> {:error, :bad_credentials}
    end
  end

  defp create_session(%{username: username, password: password}, _socket) do
    case SessionService.create_session(username, password) do
      {:ok, session} -> {:ok, session.id}
      {:error, _} = e -> e
    end
  end
end
