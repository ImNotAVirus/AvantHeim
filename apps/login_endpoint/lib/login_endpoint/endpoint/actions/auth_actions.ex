defmodule LoginEndpoint.Endpoint.AuthActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias CachingService.Player.Session
  alias Core.Socket
  alias DatabaseService.Players.{Account, Accounts}
  alias LoginEndpoint.Endpoint.Views

  @typep packet_args_75 :: %{
           username: String.t(),
           password: String.t(),
           installation_guid: String.t(),
           region_code: non_neg_integer,
           client_version: String.t(),
           client_checksum: String.t()
         }

  @typep packet_args_77 :: %{
           token: String.t(),
           installation_guid: String.t(),
           region_code: non_neg_integer,
           client_version: String.t(),
           client_checksum: String.t()
         }

  @typep packet_args :: packet_args_75() | packet_args_77()

  ## Public API

  @doc """
  Header can be "NoS0575" or "NoS0577"
  """
  @spec login(String.t(), packet_args(), Socket.t()) :: any
  def login(_header, args, socket) do
    render =
      with {:ok, _client_version} <- check_client_version(args, socket),
           {:ok, _client_checksum} <- check_client_checksum(args, socket),
           {:ok, _guid} <- check_guid(args, socket),
           {:ok, account} <- check_credentials(args, socket),
           {:ok, session_id} <- create_session(account, socket) do
        Logger.debug("Authentication succeed (username: #{account.username})")
        Views.render(:login_succeed, %{username: account.username, session_id: session_id})
      else
        reason -> render_error(reason, args)
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

  defp check_guid(%{installation_guid: guid}, _socket) do
    # Currently unused
    # TODO: Can be saved later in database to check multiclients/multiaccounts for example
    # Or if user is banned
    Logger.debug("GUID: #{guid}")
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

  if Mix.env() == :dev do
    # If env == dev: use encryption_key = 0
    defp create_session(%Account{id: id, username: username, hashed_password: password}, _socket) do
      case CachingService.create_session(username, password, id, 0) do
        {:ok, %Session{encryption_key: key}} -> {:ok, key}
        {:error, :already_exists} -> {:error, :already_connected}
      end
    end
  else
    defp create_session(%Account{username: username, hashed_password: password}, _socket) do
      case CachingService.create_session(username, password, id) do
        {:ok, %Session{encryption_key: key}} -> {:ok, key}
        {:error, :already_exists} -> {:error, :already_connected}
      end
    end
  end

  defp render_error(reason, args) do
    case reason do
      {:error, :client_version} ->
        Logger.warn("Invalid client version (got: #{args.client_version})")
        Views.render(:login_error, %{error: :old_client})

      {:error, :client_checksum} ->
        Logger.warn("Invalid client checksum (got: #{args.client_checksum})")
        Views.render(:login_error, %{error: :old_client})

      {:error, :bad_credentials} ->
        Logger.warn("Invalid credentials")
        Views.render(:login_error, %{error: :bad_credentials})

      {:error, :already_connected} ->
        user = Map.get(args, :username) || Map.get(args, :token)
        Logger.warn("Already connected (username: #{user})")
        Views.render(:login_error, %{error: :already_connected})

      e ->
        Logger.warn("Got unknown login error: #{inspect(e)}")
        Views.render(:login_error, %{})
    end
  end
end
