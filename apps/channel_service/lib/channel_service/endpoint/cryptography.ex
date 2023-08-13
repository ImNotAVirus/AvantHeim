defmodule ChannelService.Endpoint.Cryptography do
  @moduledoc """
  Cryptography for a NosTale channel endpoint.
  """

  ###
  ### TODO: THIS MODULE NEED REFACTORING !
  ###

  import Bitwise, only: [{:"^^^", 2}, {:&&&, 2}, {:>>>, 2}, {:"~~~", 1}, band: 2, bxor: 2, bsr: 2]

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

  @doc """
  Decrypt a channel packet.

  ## Examples
      iex> ChannelService.Endpoint.Cryptography.decrypt(<<198, 228, 203, 145, 70, 205, 214, 220, 208, 217, 208, 196, 7, 212, 73, 255, 208, 203, 222, 209, 215, 208, 210, 218, 193, 112, 67, 220, 208, 210, 63, 199, 228, 203, 161, 16, 72, 215, 214, 221, 200, 214, 200, 214, 248, 193, 160, 65, 218, 193, 224, 66, 241, 205>>, %{})
      "7391784-.37:83898 868 71;481.6; 8 788;8-848 8.877-2 .0898 8.. 7491785-  .584838:75837583:57-5 .-877-9 ..:-7:"

      iex> ChannelService.Endpoint.Cryptography.decrypt(<<159, 172, 100, 160, 99, 235, 103, 120, 99, 14>>, %{})
      "5 59115 1098142510;;"
  """
  @spec decrypt(binary, map) :: binary
  def decrypt(binary, %{encryption_key: encryption_key}) when not is_nil(encryption_key) do
    mode = bsr(encryption_key, band(6, 0x03))
    offset = band(encryption_key, 0xFF) + band(0x40, 0xFF)
    do_decrypt_channel(binary, mode, offset)
  end

  def decrypt(binary, _) do
    for <<c <- binary>>, into: <<>>, do: do_decrypt_session(c)
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

  @spec world_xor(raw :: binary, encryption_key :: integer, is_key_packet :: boolean) :: binary
  defp world_xor(binary, _, true) do
    for <<c <- binary>>, into: "", do: do_world_xor(c, -1, -1)
  end

  defp world_xor(binary, encryption_key, false) do
    decryption_type = encryption_key >>> 6 &&& 3
    offset = encryption_key &&& 0xFF
    for <<c <- binary>>, into: "", do: do_world_xor(c, offset, decryption_type)
  end

  @spec do_unpack(binary, [<<_::8>>, ...], [binary]) :: packet
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

  #
  # Private functions
  #

  defp do_decrypt_session(c) do
    first_byte = c - 0xF
    second_byte = band(first_byte, 0xF0)
    first_key = first_byte - second_byte
    second_key = bsr(second_byte, 0x4)

    for key <- [second_key, first_key], into: <<>> do
      case key do
        0 -> <<0x20>>
        1 -> <<0x20>>
        2 -> <<0x2D>>
        3 -> <<0x2E>>
        _ -> <<0x2C + key>>
      end
    end
  end

  defp do_decrypt_channel(c, mode, offset) do
    case mode do
      0 -> <<c - offset>>
      1 -> <<c + offset>>
      2 -> <<bxor(c - offset, 0xC3)>>
      3 -> <<bxor(c + offset, 0xC3)>>
    end
  end
end
