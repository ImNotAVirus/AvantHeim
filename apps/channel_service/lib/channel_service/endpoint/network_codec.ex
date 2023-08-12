defmodule ChannelService.Endpoint.NetworkCodec do
  @moduledoc """
  TODO: ChannelService.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  # alias ElvenPackets.Client.ChannelPackets
  # alias ChannelService.Endpoint.Cryptography

  @impl true
  def next(<<>>), do: {nil, <<>>}

  def next(message) do
    raise "unimplemented next for #{inspect(message, limit: :infinity)}"
    # {message, ""}
  end

  @impl true
  def deserialize(_raw, _socket) do
    raise "unimplemented deserialize"
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
