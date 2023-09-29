defmodule ChannelService.Endpoint.Protocol do
  @moduledoc false

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 2]

  alias ElvenPackets.Views.{
    ChatViews,
    EntityViews,
    PlayerViews,
    MapViews,
    UIViews,
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

    {:ok,
     assign(socket, state: :handshake, offset: nil, mode: nil, delimiter: nil, username: nil)}
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

  ## Map events

  @impl true
  def handle_info(:map_leave, socket) do
    Socket.send(socket, MapViews.render(:mapout))
    {:noreply, socket}
  end

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

  def handle_info({:entity_map_leave, entity_type, entity_id}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id}
    Socket.send(socket, VisibilityViews.render(:out, attrs))
    {:noreply, socket}
  end

  def handle_info({:direction_changed, entity_type, entity_id, value}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id, direction: value}
    Socket.send(socket, EntityViews.render(:dir, attrs))
    {:noreply, socket}
  end

  def handle_info({:entity_move, entity_type, entity_id, map_x, map_y, speed}, socket) do
    attrs = %{
      entity_type: entity_type,
      entity_id: entity_id,
      map_x: map_x,
      map_y: map_y,
      speed: speed
    }

    Socket.send(socket, MapViews.render(:mv, attrs))

    {:noreply, socket}
  end

  ## Entity events

  def handle_info({:update_condition, entity_type, entity_id, no_attack, no_move, speed}, socket) do
    attrs = %{
      entity_type: entity_type,
      entity_id: entity_id,
      no_attack: no_attack,
      no_move: no_move,
      speed: speed
    }

    Socket.send(socket, EntityViews.render(:cond, attrs))
    {:noreply, socket}
  end

  ## Chat messages

  def handle_info({:chat_message, entity_type, entity_id, message}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id, message: message}
    Socket.send(socket, ChatViews.render(:say, attrs))
    {:noreply, socket}
  end

  def handle_info({:chat_message, entity_type, entity_id, color, message}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id, color: color, message: message}
    Socket.send(socket, ChatViews.render(:say, attrs))
    {:noreply, socket}
  end

  ## UI

  def handle_info({:open_bank_window, gold, bank_gold, bank_rank, bank_tax}, socket) do
    # Open an empty bank widget
    attrs = %{
      action_type: :open_from_savings_book,
      gold: gold,
      bank_gold: bank_gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    }

    Socket.send(socket, UIViews.render(:gb, attrs))

    # Text: Balance: %s Golds; Carrying: %s Gold
    attrs = %{i18n_key: "BalanceBank", bank_gold: bank_gold, gold: gold}
    Socket.send(socket, UIViews.render(:s_memoi2, attrs))

    # Text: We'll do our best. Thank you for using the Cuarry Bank.
    Socket.send(socket, UIViews.render(:s_memoi, %{i18n_key: "ThankYouForUsingTheCuarryBank"}))

    {:noreply, socket}
  end

  def handle_info({:show_effect, entity_type, entity_id, value}, socket) do
    attrs = %{entity_type: entity_type, entity_id: entity_id, value: value}
    Socket.send(socket, EntityViews.render(:eff, attrs))
    {:noreply, socket}
  end

  def handle_info({:update_gold, gold, bank_gold}, socket) do
    attrs = %{gold: gold, bank_gold: bank_gold}
    Socket.send(socket, UIViews.render(:gold, attrs))
    {:noreply, socket}
  end

  ## Default handlers

  def handle_info(msg, socket) do
    Logger.warn("unhandled message: #{inspect(msg)}")
    {:noreply, socket}
  end
end
