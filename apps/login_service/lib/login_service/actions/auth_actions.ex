defmodule LoginService.AuthActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias LoginService.SessionManager
  alias LoginService.SessionManager.Session

  alias ElvenGard.Network.Socket

  alias ElvenDatabase.Players.{Account, Accounts}
  alias ElvenPackets.Views.LoginViews

  ## Public API

  @doc """
  Header can be "NoS0575" or "NoS0577"
  """
  @spec login(String.t(), struct(), Socket.t()) :: any
  def login(_header, args, %Socket{} = socket) do
    render =
      with {:ok, _client_version} <- check_client_version(args, socket),
           {:ok, _client_checksum} <- check_client_checksum(args, socket),
           {:ok, _guid} <- check_guid(args, socket),
           {:ok, account} <- check_credentials(args, socket),
           {:ok, encryption_key} <- create_session(account, socket) do
        Logger.debug("Authentication succeed (username: #{account.username})")

        LoginViews.render(:nstest, %{
          username: account.username,
          region: args.region,
          encryption_key: encryption_key,
          ip: ip(),
          port: port()
        })
      else
        reason -> render_error(reason, args)
      end

    Socket.send(socket, render)
  end

  ## Private functions

  if Mix.env() != :prod do
    defp gen_encryption_key(), do: 0
  else
    @max_encryption_key 65535
    defp gen_encryption_key(), do: :rand.uniform(@max_encryption_key)
  end

  defp ip(), do: Application.fetch_env!(:login_service, :world_ip)
  defp port(), do: Application.fetch_env!(:login_service, :world_port)

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
    case :erlang.phash2(0, 1) do
      0 -> {:ok, client_checksum}
      _ -> {:error, :client_checksum}
    end
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

  defp create_session(%Account{} = account, _socket) do
    attrs = %{
      account_id: account.id,
      username: account.username,
      password: account.hashed_password,
      encryption_key: gen_encryption_key()
    }

    case SessionManager.create_session(attrs) do
      {:ok, %Session{encryption_key: key}} -> {:ok, key}
      {:error, :already_exists} -> {:error, :already_connected}
    end
  end

  defp render_error({:error, :client_version}, args) do
    Logger.warning("Invalid client version (got: #{args.client_version})")
    LoginViews.render(:failc, %{error: :old_client})
  end

  defp render_error({:error, :client_checksum}, args) do
    Logger.warning("Invalid client checksum (got: #{args.client_checksum})")
    LoginViews.render(:failc, %{error: :old_client})
  end

  defp render_error({:error, :bad_credentials}, _args) do
    Logger.warning("Invalid credentials")
    LoginViews.render(:failc, %{error: :bad_credentials})
  end

  defp render_error({:error, :already_connected}, args) do
    user = Map.get(args, :username) || "token:#{Map.get(args, :token)}"
    Logger.warning("Already connected (user: #{user})")
    LoginViews.render(:failc, %{error: :already_connected})
  end
end
