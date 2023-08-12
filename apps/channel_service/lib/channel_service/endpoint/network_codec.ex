defmodule ChannelService.Endpoint.NetworkCodec do
  @moduledoc """
  TODO: ChannelService.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  # alias ElvenPackets.Client.ChannelPackets
  alias ChannelService.Endpoint.Cryptography

  ## Helpers

  defguardp has_state(socket, state) when socket.assigns.state == state

  ## Behaviour impls

  @impl true
  def next(raw, socket) do
    Cryptography.next(raw, socket.assigns.enc_key)
  end

  @impl true
  def deserialize(raw, socket) when has_state(socket, :handshake) do
    packet =
      raw
      |> Cryptography.unpack()
      |> String.split(" ")
      # Remove packet unique id
      |> Enum.drop(1)

    {:handshake, packet}
  end

  def deserialize(raw, _socket) do
    _packet =
      raw
      |> Cryptography.unpack()
      |> String.split(" ", parts: 3)
      # Remove packet unique id
      |> Enum.drop(1)

    # case packet do
    #   [packet_id] -> ChannelPackets.deserialize(packet_id, "", socket)
    #   [packet_id, params] -> ChannelPackets.deserialize(packet_id, params, socket)
    # end
  end

  @impl true
  def serialize(struct, socket) when is_struct(struct) do
    {packet_id, params} = struct.__struct__.serialize(struct)
    serialize([packet_id, params], socket)
  end

  def serialize(raw, _socket) when is_list(raw) do
    raw
    |> List.flatten()
    |> Enum.intersperse(" ")
    |> :erlang.list_to_binary()
    |> Cryptography.encrypt()
  end
end
