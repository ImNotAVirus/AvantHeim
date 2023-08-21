defmodule LoginService.Endpoint.NetworkCodec do
  @moduledoc """
  TODO: LoginService.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  alias ElvenPackets.Client.LoginPackets
  alias LoginService.Endpoint.Cryptography

  @impl true
  def next(<<>>, _socket), do: {nil, <<>>}
  def next(message, _socket), do: {message, ""}

  @impl true
  def decode(raw, socket) do
    decrypted = raw |> Cryptography.decrypt(socket.assigns) |> String.trim_trailing("\n")
    [packet_id, rest] = String.split(decrypted, " ", parts: 2)
    LoginPackets.deserialize(packet_id, rest, socket)
  end

  @impl true
  def encode(struct, socket) when is_struct(struct) do
    {packet_id, params} = struct.__struct__.serialize(struct)
    encode([packet_id, params], socket)
  end

  def encode(raw, _socket) when is_list(raw) do
    raw
    |> List.flatten()
    |> Enum.intersperse(" ")
    |> :erlang.list_to_binary()
    |> Cryptography.encrypt()
  end
end
