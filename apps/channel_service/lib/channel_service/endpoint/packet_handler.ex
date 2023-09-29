defmodule ChannelService.Endpoint.PacketHandler do
  @moduledoc """
  TODO: ChannelService.Endpoint.PacketHandler
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias GameService.PlayerBundle
  alias GameService.Events, as: Evt

  alias ElvenPackets.Client.{
    AreaPackets,
    LobbyPackets
  }

  @behaviour ElvenGard.Network.PacketHandler

  @mix_env Mix.env()

  ## Special handlers without packet headers

  # First packet received: encryption key
  def handle_packet({:handshake, [encryption_key]}, socket)
      when is_nil(socket.assigns.encryption_key) do
    encryption_key = String.to_integer(encryption_key)

    if encryption_key == 0 and @mix_env == :prod do
      Logger.warn("Encryption key is 0", socket_id: socket.id)
    end

    offset = ChannelService.Endpoint.Cryptography.cipher_offset(encryption_key)
    mode = ChannelService.Endpoint.Cryptography.cipher_mode(encryption_key)
    delimiter = ChannelService.Endpoint.Cryptography.pack_delimiter(offset, mode)

    socket =
      socket
      |> assign(:offset, offset)
      |> assign(:mode, mode)
      |> assign(:delimiter, delimiter)

    {:cont, socket}
  end

  # Second packet received: username
  def handle_packet({:handshake, [username, "ORG", "0"]}, socket) do
    {:cont, assign(socket, :username, username)}
  end

  # Third packet received: password
  def handle_packet({:handshake, [password]}, socket) do
    params = %{username: socket.assigns.username, password: password}

    case ChannelService.AuthActions.handshake(:handshake, params, socket) do
      {:cont, socket} -> {:cont, assign(socket, :state, :ok)}
      {:halt, _} = result -> result
    end
  end

  ## Lobby handlers

  def handle_packet(%LobbyPackets.Select{} = packet, socket) do
    ChannelService.LobbyActions.select_character("select", packet, socket)
  end

  def handle_packet(%LobbyPackets.GameStart{} = packet, socket) do
    {:ok, bundle} = ChannelService.GameActions.game_start("game_start", packet, socket)

    new_socket =
      socket
      |> Map.update!(:assigns, &Map.delete(&1, :character))
      |> assign(:map_ref, PlayerBundle.map_ref(bundle))

    {:cont, new_socket}
  end

  ## Map handlers

  def handle_packet(%AreaPackets.Dir{} = packet, socket) do
    %AreaPackets.Dir{
      entity_type: entity_type,
      entity_id: entity_id,
      direction: direction
    } = packet

    # Send dir to the game engine
    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityChangeDirection{
          entity_type: entity_type,
          entity_id: entity_id,
          value: direction
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  def handle_packet(%AreaPackets.Walk{} = packet, socket) do
    %AreaPackets.Walk{pos_x: pos_x, pos_y: pos_y, speed: speed, checksum: checksum} = packet

    # Send walk to the game engine
    {:ok, _events} =
      ElvenGard.ECS.push(
        %Evt.EntityMove{
          entity_type: :player,
          entity_id: socket.assigns.character_id,
          pos_x: pos_x,
          pos_y: pos_y,
          speed: speed,
          checksum: checksum
        },
        partition: socket.assigns.map_ref
      )

    {:cont, socket}
  end

  ## Default handler

  def handle_packet(:ignore, socket), do: {:cont, socket}

  def handle_packet(packet, socket) do
    Logger.warn("unimplemented handler for #{inspect(packet)}")
    {:cont, socket}
  end
end
