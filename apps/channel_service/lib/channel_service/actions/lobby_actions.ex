defmodule ChannelService.LobbyActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias ElvenGard.Network.Socket
  alias ElvenDatabase.Players.{Account, Characters}
  alias ElvenPackets.Views.LobbyViews

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
    %Account{id: account_id} = account

    new_socket =
      case Characters.get_by_account_id_and_slot(account_id, slot) do
        nil ->
          Logger.warn("Invalid character slot", socket_id: socket.id)
          socket

        character ->
          Socket.send(socket, LobbyViews.render(:ok))

          # Temporary store the character (deleted when you enter in game)
          Socket.assign(socket, character_id: character.id, character: character)
      end

    {:cont, new_socket}
  end
end
