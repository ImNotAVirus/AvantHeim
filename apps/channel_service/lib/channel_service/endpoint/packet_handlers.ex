defmodule ChannelService.Endpoint.PacketHandlers do
  @moduledoc """
  TODO: ChannelService.Endpoint.PacketHandlers
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

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

  ## Normal handlers

  def handle_packet(packet, socket) do
    Logger.warn("unimplemented handler for #{inspect(packet)}")
    {:cont, socket}
  end
end
