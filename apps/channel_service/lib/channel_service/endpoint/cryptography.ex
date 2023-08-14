defmodule ChannelService.Endpoint.Cryptography do
  @moduledoc """
  Cryptography for a NosTale channel endpoint.
  """

  ###
  ### TODO: THIS MODULE NEED REFACTORING !
  ###

  import Bitwise, only: [band: 2, bxor: 2, bsr: 2, bnot: 1]

  @table ["\0", " ", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "\n", "\0"]

  @typep packet() :: String.t()

  ## Public API

  @doc """
  Decrypt the delimiter from a key.
  """
  @spec decrypt_delimiter(integer()) :: integer()
  def decrypt_delimiter(key) do
    offset = decrypt_offset(key)
    mode = decrypt_mode(key)

    case mode do
      0 -> 0xFF + offset
      1 -> 0xFF - offset
      2 -> bxor(0xFF + offset, 0xC3)
      3 -> bxor(0xFF - offset, 0xC3)
    end
  end

  @doc """
  Decrypt the offset from a key.
  """
  @spec decrypt_offset(integer()) :: integer()
  def decrypt_offset(key) do
    band(key, 0xFF)
  end

  @doc """
  Decrypt the mode from a key.
  """
  @spec decrypt_mode(integer()) :: integer()
  def decrypt_mode(key) do
    bsr(key, band(6, 3))
  end

  @doc """
  Get the next packet from a raw binary.

  ## Examples

      iex> ChannelService.Endpoint.Cryptography.next(<<198, 228, 203, 145, 70, 205, 214, 220, 208, 217, 208, 196, 7, 212, 73, 255, 208, 203, 222, 209, 215, 208, 210, 218, 193, 112, 67, 220, 208, 210, 63, 199, 228, 203, 161, 16, 72, 215, 214, 221, 200, 214, 200, 214, 248, 193, 160, 65, 218, 193, 224, 66, 241, 205, 199, 228, 203, 161, 16, 72, 215, 214, 221, 200, 214, 200, 214, 248, 193, 160, 65, 218, 193, 224, 66, 241, 205>>, 0xFF)
      {<<198, 228, 203, 145, 70, 205, 214, 220, 208, 217, 208, 196, 7, 212, 73>>, <<208, 203, 222, 209, 215, 208, 210, 218, 193, 112, 67, 220, 208, 210, 63, 199, 228, 203, 161, 16, 72, 215, 214, 221, 200, 214, 200, 214, 248, 193, 160, 65, 218, 193, 224, 66, 241, 205, 199, 228, 203, 161, 16, 72, 215, 214, 221, 200, 214, 200, 214, 248, 193, 160, 65, 218, 193, 224, 66, 241, 205>>}
  """
  @spec next(binary(), integer(), binary()) :: {binary() | nil, binary()}
  def next(raw, delimiter, acc \\ <<>>)

  def next(<<>>, delimiter, acc) do
    {acc, <<>>}
  end

  def next(<<c, rest::binary>>, delimiter, acc) do
    if c == delimiter do
      {acc, rest}
    else
      next(rest, delimiter, <<acc::binary, c>>)
    end
  end

  @spec unpack(binary(), any()) :: packet()
  def unpack(binary, _ \\ nil) do
    do_unpack(binary, @table)
  end

  @doc """
  Encrypt a world packet.

  ## Examples

      iex> ChannelService.Endpoint.Cryptography.encrypt("foo")
      <<3, 153, 144, 144, 255>>
  """
  @spec encrypt(binary) :: binary
  def encrypt(packet) do
    bytes =
      packet
      |> :binary.bin_to_list()
      |> Enum.with_index()

    len = length(bytes)

    data =
      for {c, i} <- bytes, into: <<>> do
        if rem(i, 0x7E) != 0 do
          <<bnot(c)>>
        else
          remaining = if len - i > 0x7E, do: 0x7E, else: len - i
          <<remaining, bnot(c)>>
        end
      end

    <<data::binary, 0xFF>>
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
    mode = decrypt_mode(encryption_key)
    offset = decrypt_offset(encryption_key)
    do_decrypt_channel(binary, mode, offset)
  end

  def decrypt(binary, _) do
    do_decrypt_session(binary)
  end

  ## Private functions

  @spec do_unpack(binary, [<<_::8>>, ...], [binary]) :: packet
  defp do_unpack(binary, chars_to_unpack, result \\ [])

  defp do_unpack("", _, result) do
    result
    |> Enum.reverse()
    |> :unicode.characters_to_binary(:latin1)
  end

  defp do_unpack(<<byte::size(8), rest::binary>>, chars_to_unpack, result) do
    is_packed = band(byte, 0x80) > 0
    tmp_len = band(byte, 0x7F)
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
    for <<c <- chunk>>, into: "", do: <<bxor(c, 0xFF)>>
  end

  defp decode_chunk(chunk, chars_to_unpack, true) do
    for <<h::size(4), l::size(4) <- chunk>>, into: "" do
      left_byte = Enum.at(chars_to_unpack, h)
      right_byte = Enum.at(chars_to_unpack, l)
      if l != 0, do: left_byte <> right_byte, else: left_byte
    end
  end

  defp do_decrypt_session(<<>>) do
    <<>>
  end

  defp do_decrypt_session(<<c>>) do
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

  defp do_decrypt_session(packet) do
    for <<c <- packet>>, into: <<>>, do: do_decrypt_session(<<c>>)
  end

  defp do_decrypt_channel(c, mode, offset) do
    case mode do
      0 -> <<c - offset>>
      1 -> <<c + offset>>
      2 -> <<bxor(c - offset, 0xC3)>>
      3 -> <<bxor(c + offset, 0xC3)>>
    end
  end

  defp do_unpack_linear(packet, flag) do
    len =
      if flag < length(packet) do
        flag
      else
        length(packet)
      end

    for <<c <- :binary.part(packet, {0, len})>>, into: <<>> do
      <<bxor(c, 0xFF)>>
    end
  end
end
