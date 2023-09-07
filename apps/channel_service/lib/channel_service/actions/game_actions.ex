defmodule ChannelService.GameActions do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Network.Socket
  alias ElvenCaching.CharacterRegistry
  alias ElvenPackets.Views.{ChatViews, PlayerViews, UIViews}

  alias ChannelService.EntityInteractions

  alias ElvenGard.ECS.Command
  alias GameService.PlayerEntity

  ## Packet handlers

  @spec game_start(String.t(), map, Socket.t()) :: {:cont, Socket.t()}
  def game_start("game_start", _, %Socket{} = socket) do
    %{character_id: character_id} = socket.assigns
    {:ok, character} = CharacterRegistry.get(character_id)

    account = socket.assigns.account
    {:ok, _entity} = Command.spawn_entity(PlayerEntity.new(character, account, self()))

    # ----------------
    Socket.send(socket, PlayerViews.render(:tit, %{character: character}))
    Socket.send(socket, PlayerViews.render(:fd, %{character: character}))
    # TODO: Socket.send(socket, PlayerViews.render(:ski, %{character: character}))

    EntityInteractions.send_map_enter(character)

    Socket.send(socket, PlayerViews.render(:rsfi))
    Socket.send(socket, PlayerViews.render(:fs, %{character: character}))

    Socket.send(socket, UIViews.render(:gold, %{character: character}))

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
