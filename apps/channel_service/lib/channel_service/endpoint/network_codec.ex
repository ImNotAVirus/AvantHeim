defmodule ChannelService.Endpoint.NetworkCodec do
  @moduledoc """
  TODO: ChannelService.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  require Logger
  alias ElvenPackets.Client.WorldPackets
  alias ChannelService.Endpoint.Cryptography

  ## Ignore some packet
  ## FIXME: Clean this
  @ignore ["0", "c_close", "f_stash_end", "lbs"]

  ## Helpers

  defguardp has_state(socket, state) when socket.assigns.state == state

  ## Behaviour impls

  @impl true
  def next(raw, socket) do
    Cryptography.next(raw, socket.assigns.enc_key)
  end

  @impl true
  def decode(raw, socket) when has_state(socket, :handshake) do
    packet =
      raw
      |> Cryptography.unpack()
      |> String.split(" ")
      # Remove packet unique id
      |> Enum.drop(1)

    {:handshake, packet}
  end

  def decode(raw, socket) do
    case unpack_packet(raw) do
      [packet_id | _] when packet_id in @ignore -> :ignore
      ["/" <> player, message] -> WorldPackets.deserialize("whisper", player_name <> " " <> message, socket)
      [packet_id] -> WorldPackets.deserialize(packet_id, "", socket)
      [packet_id, params] -> WorldPackets.deserialize(packet_id, params, socket)
    end
  catch
    :error, :function_clause ->
      packet = unpack_packet(raw)
      Logger.warn("no serializer found for #{inspect(packet)}")
      :ignore
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

  ## Private helpers

  defp unpack_packet(packet) do
    packet
    |> Cryptography.unpack()
    |> String.split(" ", parts: 3)
    # Remove packet unique id
    |> Enum.drop(1)
  end
end
