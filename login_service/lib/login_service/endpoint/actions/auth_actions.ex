defmodule LoginService.Endpoint.AuthActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias ElvenCaching.Account.Session
  alias ElvenCaching.SessionRegistry
  alias ElvenCore.Socket
  alias ElvenDatabase.Players.{Account, Accounts}
  alias ElvenViews.LoginViews

  @ip Application.fetch_env!(:login_service, :world_ip)
  @port Application.fetch_env!(:login_service, :world_port)

  # If env != prod: use encryption_key = 0
  @default_encryption_key if Mix.env() == :prod, do: nil, else: 0
  @max_encryption_key 65535

  ## Public API

  @doc """
  Header can be "NoS0575" or "NoS0577"
  """
  @spec login(String.t(), map, Socket.t()) :: any
  def login(_header, args, %Socket{id: socket_id} = socket) do
    render =
      with {:ok, _client_version} <- check_client_version(args, socket),
           {:ok, _client_checksum} <- check_client_checksum(args, socket),
           {:ok, _guid} <- check_guid(args, socket),
           {:ok, account} <- check_credentials(args, socket),
           {:ok, encryption_key} <- create_session(account, socket) do
        Logger.debug("Authentication succeed for #{socket_id} (username: #{account.username})")

        LoginViews.render(:login_succeed, %{
          username: account.username,
          encryption_key: encryption_key,
          ip: @ip,
          port: @port
        })
      else
        reason -> render_error(reason, args, socket_id)
      end

    Socket.send(socket, render)
  end

  ## Private functions

  defp check_client_version(%{client_version: version}, _socket) do
    requirement = Application.fetch_env!(:login_service, :client_version)

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

  defp check_guid(%{installation_guid: guid}, socket) do
    # Currently unused
    # TODO: Can be saved later in database to check multiclients/multiaccounts for example
    # Or if user is banned
    Logger.debug("GUID for #{socket.id}: #{guid}")
    {:ok, guid}
  end

  defp check_credentials(%{token: token}, socket) do
    [decoded_token, username] = token |> Base.decode16!() |> String.split(":", parts: 2)

    # TODO: Finish the launcher and use caching service
    if decoded_token == "deadbeef" do
      password = :crypto.hash(:sha512, username) |> Base.encode16()
      check_credentials(%{username: username, password: password}, socket)
    else
      {:error, :bad_credentials}
    end
  end

  defp check_credentials(%{username: username, password: password}, _socket) do
    case Accounts.log_in(username, password) do
      %Account{} = account -> {:ok, account}
      nil -> {:error, :bad_credentials}
    end
  end

  defp create_session(%Account{} = account, _socket) do
    attrs = %{
      account_id: account.id,
      username: account.username,
      password: account.password,
      # Little trick for avoid Elixir warning 
      encryption_key:
        (:erlang.phash2(0, 1) == 0 and @default_encryption_key) ||
          :rand.uniform(@max_encryption_key)
    }

    case SessionRegistry.create(attrs) do
      {:ok, %Session{encryption_key: key}} -> {:ok, key}
      {:error, :already_exists} -> {:error, :already_connected}
    end
  end

  defp render_error(reason, args, socket_id) do
    case reason do
      {:error, :client_version} ->
        Logger.warn("Invalid client version (got: #{args.client_version})", socket_id: socket_id)
        LoginViews.render(:login_error, %{error: :old_client})

      {:error, :client_checksum} ->
        Logger.warn("Invalid client checksum (got: #{args.client_checksum})", socket_id: socket_id)

        LoginViews.render(:login_error, %{error: :old_client})

      {:error, :bad_credentials} ->
        Logger.warn("Invalid credentials", socket_id: socket_id)
        LoginViews.render(:login_error, %{error: :bad_credentials})

      {:error, :already_connected} ->
        user = Map.get(args, :username) || Map.get(args, :token)
        Logger.warn("Already connected (username: #{user})", socket_id: socket_id)
        LoginViews.render(:login_error, %{error: :already_connected})

      e ->
        Logger.warn("Got unknown login error: #{inspect(e)}", socket_id: socket_id)
        LoginViews.render(:login_error, %{})
    end
  end
end
