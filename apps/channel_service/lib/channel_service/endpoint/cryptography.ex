defmodule ChannelService.Endpoint.Cryptography do
  @moduledoc """
  Cryptography for a NosTale channel endpoint.
  """

  ###
  ### TODO: THIS MODULE NEED REFACTORING !
  ###

  import Bitwise, only: [{:"^^^", 2}, {:&&&, 2}, {:>>>, 2}, {:"~~~", 1}]

  @table ["\0", " ", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "\n", "\0"]

  ## Public API

  @doc """
  Encrypt a world packet.

  ## Examples

  """
  @spec encrypt(String.t()) :: binary
  def encrypt(packet) do
    bytes =
      packet
      |> :unicode.characters_to_list(:utf8)
      |> Enum.with_index()

    length = length(bytes)
    data = for {b, i} <- bytes, into: <<>>, do: do_encrypt(b, i, length)
    <<data::binary, 0xFF::size(8)>>
  end

  @doc """
  Decrypt a channel packet.
  """
  @spec decrypt(binary, map) :: [String.t()]
  def decrypt(binary, %{encryption_key: encryption_key}) when not is_nil(encryption_key),
    do: decrypt_channel(binary, encryption_key)

  def decrypt(binary, _), do: decrypt_session(binary)

  ## Private functions

  @typep packet() :: String.t()

  @spec decrypt_session(binary) :: [String.t()]
  defp decrypt_session(binary) do
    binary
    |> world_xor(-1, true)
    |> unpack(@table)
    |> split_keepalive(true)
    |> Enum.at(0)
    |> Kernel.elem(1)
  end

  @spec decrypt_channel(binary, integer, boolean) :: [binary | {integer, binary}]
  defp decrypt_channel(binary, encryption_key, _remove_keepalive? \\ true) do
    binary
    |> world_xor(encryption_key, false)
    |> unpack(@table)
    # |> split_keepalive(keepalive?)
    |> remove_keepalive()
  end

  @spec world_xor(raw :: binary, encryption_key :: integer, is_key_packet :: boolean) :: binary
  defp world_xor(binary, _, true) do
    for <<c <- binary>>, into: "", do: do_world_xor(c, -1, -1)
  end

  defp world_xor(binary, encryption_key, false) do
    decryption_type = encryption_key >>> 6 &&& 3
    offset = encryption_key &&& 0xFF
    for <<c <- binary>>, into: "", do: do_world_xor(c, offset, decryption_type)
  end

  @spec do_world_xor(pos_integer, integer, integer) :: binary
  defp do_world_xor(char, offset, 0), do: <<char - offset - 0x40 &&& 0xFF>>
  defp do_world_xor(char, offset, 1), do: <<char + offset + 0x40 &&& 0xFF>>
  defp do_world_xor(char, offset, 2), do: <<(char - offset - 0x40) ^^^ 0xC3 &&& 0xFF>>
  defp do_world_xor(char, offset, 3), do: <<(char + offset + 0x40) ^^^ 0xC3 &&& 0xFF>>
  defp do_world_xor(char, _, _), do: <<char - 0x0F &&& 0xFF>>

  @spec unpack(binary, [<<_::8>>, ...]) :: [packet]
  defp unpack(binary, chars_to_unpack) do
    binary
    |> :binary.split(<<0xFF>>, [:global, :trim_all])
    |> Enum.map(&do_unpack(&1, chars_to_unpack))
  end

  @spec do_unpack(binary, [<<_::8>>, ...], [binary]) :: packet
  defp do_unpack(binary, chars_to_unpack, result \\ [])

  defp do_unpack("", _, result) do
    result
    |> Enum.reverse()
    |> Enum.join()
    |> :unicode.characters_to_binary(:latin1)
  end

  defp do_unpack(<<byte::size(8), rest::binary>>, chars_to_unpack, result) do
    is_packed = (byte &&& 0x80) > 0
    tmp_len = byte &&& 0x7F
    len = if is_packed, do: ceil(tmp_len / 2), else: tmp_len

    <<chunk::bytes-size(len), next::binary>> = rest
    chunk = decode_chunk(chunk, chars_to_unpack, is_packed)

    do_unpack(next, chars_to_unpack, [chunk | result])
  end

  @doc false
  @spec decode_chunk(
          chunk :: binary,
          chars_to_unpack :: [<<_::8>>, ...],
          is_packed :: boolean
        ) :: binary
  defp decode_chunk(chunk, _, false) do
    for <<c <- chunk>>, into: "", do: <<c ^^^ 0xFF>>
  end

  defp decode_chunk(chunk, chars_to_unpack, true) do
    for <<h::size(4), l::size(4) <- chunk>>, into: "" do
      left_byte = Enum.at(chars_to_unpack, h)
      right_byte = Enum.at(chars_to_unpack, l)
      if l != 0, do: left_byte <> right_byte, else: left_byte
    end
  end

  @spec split_keepalive([packet], boolean) :: [packet] | [{integer, packet}]
  defp split_keepalive(packet, false), do: packet

  defp split_keepalive(packet, true) do
    packet
    |> Stream.map(&String.split(&1, " ", parts: 2))
    |> Enum.map(fn [l, r] -> {String.to_integer(l), r} end)
  end

  @spec remove_keepalive([packet]) :: [packet]
  defp remove_keepalive(packet) do
    packet
    |> Stream.map(&String.split(&1, " ", parts: 2))
    |> Enum.map(fn [_, packet] -> packet end)
  end

  @spec do_encrypt(char, integer, integer) :: binary
  defp do_encrypt(char, index, _) when rem(index, 0x7E) != 0, do: <<(~~~char)>>

  defp do_encrypt(char, index, length) do
    remaining = if length - index > 0x7E, do: 0x7E, else: length - index
    <<remaining::size(8), ~~~char::size(8)>>
  end
end
