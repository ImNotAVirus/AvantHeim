defmodule ChannelEndpoint.Endpoint.LobbyActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias Core.Socket
  alias CachingService.Player.Session
  alias ChannelEndpoint.Endpoint.LobbyViews
  alias DatabaseService.Players.{Account, Characters}

  ## Public API

  ## TODO: Add security check: lobby packet sent from world

  @spec create_character(String.t(), map, Socket.t()) :: any
  def create_character("Char_NEW", _args, _socket) do
    raise "unimplemented resolver"
  end

  @spec delete_character(String.t(), map, Socket.t()) :: any
  def delete_character("Char_DEL", _args, _socket) do
    raise "unimplemented resolver"
  end

  @spec select_character(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def select_character("select", %{slot: slot}, socket) do
    account = socket.assigns.account
    %Account{id: account_id, username: username} = account

    new_socket =
      case Characters.get_by_account_id_and_slot(account_id, slot) do
        nil ->
          Logger.warn("Invalid character slot", socket_id: socket.id)
          socket

        character ->
          {:ok, _} = update_session_state(username)
          {:ok, _} = CachingService.init_character(character, socket)
          Socket.send(socket, LobbyViews.render(:ok, nil))
          Socket.assign(socket, character_id: character.id)
      end

    {:cont, new_socket}
  end

  ## Private functions

  defp update_session_state(username) do
    username
    |> CachingService.get_session_by_username()
    |> then(fn {:ok, session} -> session end)
    |> Session.set_state(:in_game)
    |> CachingService.update_session()
  end
end
