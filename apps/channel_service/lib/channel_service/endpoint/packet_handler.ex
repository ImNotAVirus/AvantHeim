defmodule ChannelService.Endpoint.PacketHandler do
  @moduledoc """
  TODO: ChannelService.Endpoint.PacketHandler
  """

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  @behaviour ElvenGard.Network.PacketHandler

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
