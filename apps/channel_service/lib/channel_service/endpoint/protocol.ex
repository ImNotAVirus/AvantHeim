defmodule ChannelService.Endpoint.Protocol do
  @moduledoc false

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 2]

  alias ElvenPackets.Views.{ChatViews, PlayerViews, UIViews}

  alias ChannelService.EntityInteractions
  alias ElvenGard.Network.Socket
  alias GameService.PlayerBundle

  ## Endpoint.Protocol behaviour

  @impl true
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    Logger.metadata(socket_id: socket.id)

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :raw, reuseaddr: true)

    {:ok, assign(socket, state: :handshake, enc_key: nil, username: nil)}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message (len: #{byte_size(message)})")
    {:ok, socket}
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end

  ## GenServer behaviour

  @impl true
  def handle_info({:entity_spawn, %PlayerBundle{} = player}, socket) do
    IO.inspect(player)

    case player.id == socket.assigns.character_id do
      false ->
        EntityInteractions.send_map_enter(player, socket)
        :ok

      true ->
        Socket.send(socket, PlayerViews.render(:tit, %{entity: player}))
        Socket.send(socket, PlayerViews.render(:fd, %{entity: player}))
        # TODO: Socket.send(socket, PlayerViews.render(:ski, %{entity: player}))

        EntityInteractions.send_map_enter(player, socket)

        Socket.send(socket, PlayerViews.render(:rsfi))
        Socket.send(socket, PlayerViews.render(:fs, %{entity: player}))

        Socket.send(socket, UIViews.render(:gold, %{entity: player}))

        # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 0, entity: player}))
        # TODO: Socket.send(socket, InventoryViews.render(:qslot, %{slot_id: 1, entity: player}))

        Socket.send(socket, UIViews.render(:info, %{message: "Welcome to my World!"}))

        send_bns(socket)
        send_hello(socket, player)
    end

    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.warn("unhandled message: #{inspect(msg)}")
    {:noreply, socket}
  end

  ## Private functions

  ### In Game

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
      {:special_red, "Github: https://github.com/ImNotAVirus/AvantHeim"},
      {:special_red, "Author: DarkyZ aka. ImNotAVirus"},
      {:special_green, suffix}
    ]

    Enum.each(messages, fn {color, message} ->
      attrs = %{entity: character, color: color, message: message}
      Socket.send(socket, ChatViews.render(:say, attrs))
    end)
  end
end
