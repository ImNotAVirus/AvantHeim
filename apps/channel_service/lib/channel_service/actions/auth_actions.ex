defmodule ChannelService.AuthActions do
  @moduledoc """
  TODO: 
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias ElvenGard.Network.Socket
  alias ElvenDatabase.Players.{Account, Accounts, Characters}
  alias ElvenPackets.Views.LobbyViews

  alias ChannelService.PresenceManager

  # FIXME: Currently the session manager name is hardcoded
  # Move it somewhere else
  @session_manager {:global, LoginService.SessionManager}

  ## Public API

  def handshake(:handshake, params, socket) do
    %{username: username, password: password} = params

    with {:ok, session} <- authenticate(username, password),
         :ok <- PresenceManager.register(username, session.account_id),
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

  defp authenticate(username, password) do
    GenServer.call(@session_manager, {:authenticate, username, password})
  end

  defp get_account(session) do
    # FIXME: maybe use the Session struct here
    %{username: username, password: password} = session

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
