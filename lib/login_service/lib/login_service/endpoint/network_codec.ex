defmodule LoginService.Endpoint.NetworkCodec do
  @moduledoc """
  TODO: LoginService.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  alias LoginService.ClientPackets
  alias LoginService.Endpoint.Cryptography

  @impl true
  def next(<<>>), do: {nil, <<>>}

  def next(message) do
    {message, ""}
  end

  @impl true
  def deserialize(raw, socket) do
    decrypted = raw |> Cryptography.decrypt(socket.assigns) |> String.trim_trailing("\n")
    [packet_id, rest] = String.split(decrypted, " ", parts: 2)

    packet = ClientPackets.decode(packet_id, rest, socket)

    if is_nil(packet) do
      raise "unable to decode packet with id #{inspect(packet_id)} - #{inspect(raw)}"
    end

    packet
  end

  @impl true
  def serialize(struct, socket) when is_struct(struct) do
    {packet_id, params} = struct.__struct__.encode(struct)
    serialize([packet_id, params], socket)
  end

  def serialize(raw, _socket) when is_list(raw) do
    Enum.intersperse(raw, " ")
  end
end
