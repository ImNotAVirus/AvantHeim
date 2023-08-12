defmodule ChannelService.Endpoint.PacketHandlers do
  @moduledoc """
  TODO: ChannelService.Endpoint.PacketHandlers
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  @mix_env Mix.env()

  ## Special handlers without packet headers

  # First packet received: encryption key
  def handle_packet({:handshake, [enc_key]}, socket) when is_nil(socket.assigns.enc_key) do
    enc_key = String.to_integer(enc_key)

    if enc_key == 0 and @mix_env == :prod do
      Logger.warn("Encryption key is 0", socket_id: socket.id)
    end

    {:cont, assign(socket, :enc_key, enc_key)}
  end

  # Second packet received: username
  def handle_packet({:handshake, [username, "ORG", "0"]}, socket) do
    {:cont, assign(socket, :username, username)}
  end

  # Third packet received: password
  def handle_packet({:handshake, [_password]}, socket) do
    {:cont, socket}
  end

  ## Normal handlers

  def handle_packet(packet, _socket) do
    raise "unimplemented handler for #{inspect(packet)}"
  end
end
