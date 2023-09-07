defmodule ChannelService.AuthActions do
  @moduledoc """
  TODO: 
  """

  require Logger
  require ElvenCaching.Account.Session

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias ElvenGard.Network.Socket

  alias ElvenCaching.Account.Session
  alias ElvenCaching.SessionRegistry
  alias ElvenDatabase.Players.{Account, Accounts, Characters}
  alias ElvenPackets.Views.LobbyViews

  alias ChannelService.PresenceManager

  ## Public API

  def handshake(:handshake, params, socket) do
    %{username: username, password: password} = params

    with {:ok, session} <- SessionRegistry.get(username),
         :ok <- validate_session(session, password),
         :ok <- PresenceManager.register_username(username),
         {:ok, _} <- cache_session_as_logged(session),
         {:ok, account} <- get_account(session),
         :ok <- send_character_list(account, socket) do
      {:cont, assign(socket, :account, account)}
    else
      e ->
        Logger.warn("Invalid Handshake (reason: #{inspect(e)})")
        {:halt, socket}
    end
  end

  ## Helpers

  defp validate_session(session, password) do
    hash = :crypto.hash(:sha512, password) |> Base.encode16()

    case session do
      %Session{password: ^hash} = s when not Session.is_logged(s) -> :ok
      _ -> {:error, :invalid_session}
    end
  end

  defp cache_session_as_logged(session) do
    session
    |> Session.set_ttl(:infinity)
    |> Session.set_state(:in_lobby)
    |> SessionRegistry.write()
  end

  defp get_account(%Session{} = session) do
    %Session{username: username, password: password} = session

    case Accounts.log_in(username, password) do
      %Account{} = acc -> {:ok, acc}
      _ -> {:error, :cant_fetch_account}
    end
  end

  defp send_character_list(%Account{} = account, socket) do
    character_list = Characters.all_by_account_id(account.id)
    Socket.send(socket, LobbyViews.render(:clist_start, %{}))

    Enum.each(character_list, fn character ->
      args = %{
        character: character,
        equipments: List.duplicate(-1, 10),
        pets: []
      }

      Socket.send(socket, LobbyViews.render(:clist, args))
    end)

    Socket.send(socket, LobbyViews.render(:clist_end, %{}))
  end
end
