defmodule ChannelService.Endpoint.Cryptography do
  @moduledoc """
  Cryptography for a NosTale channel endpoint.
  """

  ###
  ### TODO: THIS MODULE NEED REFACTORING !
  ###

  import Bitwise, only: [{:"^^^", 2}, {:&&&, 2}, {:>>>, 2}, {:"~~~", 1}]

  @table ["\0", " ", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "\n", "\0"]

  @typep packet() :: String.t()

  ## Public API

  @spec next(binary(), integer() | nil) :: {binary() | nil, binary()}
  def next(raw, enc_key) do
    case do_next(raw, enc_key) do
      {packet, rest} -> {packet, rest}
      nil -> {nil, raw}
    end
  end

  @spec unpack(binary(), any()) :: packet()
  def unpack(binary, _ \\ nil) do
    do_unpack(binary, @table)
  end

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

  ## Private functions

  defp do_next(raw, enc_key, acc \\ [])
  defp do_next(<<>>, _enc_key, _acc), do: nil

  defp do_next(<<byte::8, rest::binary>>, nil, acc) do
    case do_world_xor(byte, -1, -1) do
      0xFF -> {acc |> Enum.reverse() |> :erlang.list_to_binary(), rest}
      byte -> do_next(rest, nil, [byte | acc])
    end
  end

  defp do_next(<<byte::8, rest::binary>>, enc_key, acc) do
    offset = enc_key &&& 0xFF
    decryption_type = enc_key >>> 6 &&& 3

    case do_world_xor(byte, offset, decryption_type) do
      0xFF -> {acc |> Enum.reverse() |> :erlang.list_to_binary(), rest}
      byte -> do_next(rest, enc_key, [byte | acc])
    end
  end

  defp do_world_xor(char, offset, 0), do: char - offset - 0x40 &&& 0xFF
  defp do_world_xor(char, offset, 1), do: char + offset + 0x40 &&& 0xFF
  defp do_world_xor(char, offset, 2), do: (char - offset - 0x40) ^^^ 0xC3 &&& 0xFF
  defp do_world_xor(char, offset, 3), do: (char + offset + 0x40) ^^^ 0xC3 &&& 0xFF
  defp do_world_xor(char, _, _), do: char - 0x0F &&& 0xFF

  defp do_unpack(binary, chars_to_unpack, result \\ [])

  defp do_unpack("", _, result) do
    result
    |> Enum.reverse()
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

  @spec do_encrypt(char, integer, integer) :: binary
  defp do_encrypt(char, index, _) when rem(index, 0x7E) != 0, do: <<(~~~char)>>

  defp do_encrypt(char, index, length) do
    remaining = if length - index > 0x7E, do: 0x7E, else: length - index
    <<remaining::size(8), ~~~char::size(8)>>
  end
end
