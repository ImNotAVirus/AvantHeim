defmodule ChannelService.Endpoint.Protocol do
  @moduledoc false

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 2]

  alias ElvenPackets.Views.{
    EntityViews,
    PlayerViews,
    MapViews,
    VisibilityViews
  }

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
  def handle_info({:map_change, %PlayerBundle{} = entity}, socket) do
    entity_args = %{entity: entity}
    stat_args = %{entity: entity, option: 0}
    at_args = %{entity: entity, map_music: 1}

    Socket.send(socket, PlayerViews.render(:c_info, entity_args))
    Socket.send(socket, PlayerViews.render(:lev, entity_args))
    Socket.send(socket, PlayerViews.render(:stat, stat_args))
    Socket.send(socket, MapViews.render(:at, at_args))
    Socket.send(socket, MapViews.render(:c_map, entity_args))
    # TODO: Socket.send(socket, PlayerViews.render(:sc, entity_args))
    Socket.send(socket, EntityViews.render(:c_mode, entity_args))
    Socket.send(socket, EntityViews.render(:char_sc, entity_args))
    Socket.send(socket, EntityViews.render(:cond, entity_args))

    {:noreply, socket}
  end

  def handle_info({:entity_map_enter, %PlayerBundle{} = entity}, socket) do
    Socket.send(socket, VisibilityViews.render(:in, %{entity: entity}))
    Socket.send(socket, EntityViews.render(:c_mode, %{entity: entity}))

    {:noreply, socket}
  end

  def handle_info({:direction_changed, entity_type, entity_id, value}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id, direction: value}
    Socket.send(socket, EntityViews.render(:dir, attrs))
    {:noreply, socket}
  end

  def handle_info(msg, socket) do
    Logger.warn("unhandled message: #{inspect(msg)}")
    {:noreply, socket}
  end
end
