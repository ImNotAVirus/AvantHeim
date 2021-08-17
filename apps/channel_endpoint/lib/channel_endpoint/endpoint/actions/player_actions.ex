defmodule ChannelEndpoint.Endpoint.PlayerActions do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket

  alias ChannelEndpoint.Endpoint.PacketHelpers

  alias ChannelEndpoint.Endpoint.{
    ChatViews,
    PlayerViews,
    UIViews
  }

  ## Packet handlers

  @spec game_start(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CachingService.get_character_by_id(character_id)

    Socket.send(socket, PlayerViews.render(:tit, character))
    Socket.send(socket, PlayerViews.render(:fd, character))
    # TODO: Socket.send(socket, PlayerViews.render(:ski, character))

    PacketHelpers.player_map_enter(character, socket)

    Socket.send(socket, PlayerViews.render(:rsfi, character))
    Socket.send(socket, PlayerViews.render(:fs, character))

    # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 0, character: character}))
    # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 1, character: character}))

    Socket.send(socket, UIViews.render(:info, %{message: "Welcome to my World!"}))

    send_bns(socket)
    send_hello(socket, character)

    {:cont, socket}
  end

  ## Private functions

  defp send_bns(socket) do
    messages = Enum.map(1..10, fn x -> "ElvenGard ##{x}" end)

    messages
    |> Enum.with_index()
    |> Stream.map(fn {val, i} -> %{id: i, message: val} end)
    |> Enum.each(&Socket.send(socket, ChatViews.render(:bn, &1)))
  end

  defp send_hello(socket, character) do
    prefix = String.duplicate("-", 31)
    suffix = String.duplicate("-", 82)

    messages = [
      {:special_green, "#{prefix} [ ElvenGard ] #{prefix}"},
      {:special_red, "Github: https://github.com/ImNotAVirus/Flugel-NostaleEmu"},
      {:special_red, "Author: DarkyZ aka. ImNotAVirus"},
      {:special_green, suffix}
    ]

    Enum.each(messages, fn {color, message} ->
      attrs = %{entity: character, color: color, message: message}
      Socket.send(socket, ChatViews.render(:say, attrs))
    end)
  end
end
