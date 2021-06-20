defmodule ChannelEndpoint.Endpoint.LobbyActions do
  @moduledoc """
  TODO: Documentation
  """

  require Logger

  alias Core.Socket
  # alias ChannelEndpoint.Endpoint.LobbyViews
  # alias DatabaseService.Players.{Account, Characters}

  ## Public API

  @spec create_character(String.t(), map, Socket.t()) :: any
  def create_character("Char_NEW", _args, _socket) do
    raise "unimplemented resolver"
  end

  @spec delete_character(String.t(), map, Socket.t()) :: any
  def delete_character("Char_DEL", _args, _socket) do
    raise "unimplemented resolver"
  end

  @spec select_character(String.t(), map, Socket.t()) :: any
  def select_character("select", %{slot: _slot}, socket) do
    # account = socket.assigns.account

    # %Account{
    #   id: account_id,
    #   authority: account_authority,
    #   language: account_language
    # } = account

    # new_client =
    #   case Characters.get_by_account_id_and_slot(account_id, slot) do
    #     nil -> :ko
    #     character -> :ok
    #   end

    Socket.send(socket, "OK")

    {:cont, socket}
  end

  @spec game_start(String.t(), map, Socket.t()) :: any
  def game_start("game_start", _, socket) do
    {:cont, socket}
  end
end
