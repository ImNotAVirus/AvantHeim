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
  def next(<<>>, _socket), do: {nil, <<>>}

  def next(raw, socket) when has_state(socket, :handshake) do
    Cryptography.next(raw, socket.assigns.enc_key)
  end

  def next(raw, _socket) do
    raise "unimplemented next for #{inspect(raw, limit: :infinity)}"
    # {raw, ""}
  end

  @impl true
  def deserialize(raw, socket) when has_state(socket, :handshake) do
    packet =
      raw
      |> Cryptography.unpack()
      |> IO.inspect()
      |> String.split(" ")
      # Remove keep alive
      |> Enum.drop(1)

    {:handshake, packet}
  end

  def deserialize(raw, _socket) do
    raise "unimplemented deserialize - #{inspect(raw, limit: :infinity)}"

    # decrypted = raw |> Cryptography.decrypt(socket.assigns) |> String.trim_trailing("\n")
    # [packet_id, rest] = String.split(decrypted, " ", parts: 2)
    # ChannelPackets.deserialize(packet_id, rest, socket)
  end

  @impl true
  def serialize(struct, _socket) when is_struct(struct) do
    raise "unimplemented serialize"
    # {packet_id, params} = struct.__struct__.serialize(struct)
    # serialize([packet_id, params], socket)
  end

  def serialize(raw, _socket) when is_list(raw) do
    # raw
    # |> List.flatten()
    # |> Enum.intersperse(" ")
    # |> :erlang.list_to_binary()
    # |> Cryptography.encrypt()
  end
end
