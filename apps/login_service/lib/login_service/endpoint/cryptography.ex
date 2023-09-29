defmodule LoginService.Endpoint.Cryptography do
  @moduledoc """
  Cryptography for a NosTale login server.
  """

  import Bitwise, only: [band: 2, bxor: 2]

  @doc """
  Split a login packet into the next packet and the rest of the packet.

  ## Examples

      iex> LoginService.Endpoint.Cryptography.next(<<156, 187, 159, 2, 5, 3, 5, 242, 255, 4, 1, 6, 2, 255, 10, 242, 177, 242, 5, 145, 149, 4, 0, 5, 4, 4, 5, 148, 255, 149, 2, 144, 150, 2, 145, 2, 4, 5, 149, 150, 2, 3, 145, 6, 1, 9, 10, 9, 149, 6, 2, 0, 5, 144, 3, 9, 150, 1, 255, 9, 255, 2, 145, 0, 145, 10, 143, 5, 3, 150, 4, 144, 6, 255, 0, 5, 0, 0, 4, 3, 2, 3, 150, 9, 5, 4, 145, 2, 10, 0, 150, 1, 149, 9, 1, 144, 6, 150, 9, 4, 145, 3, 9, 255, 5, 4, 0, 150, 148, 9, 10, 148, 150, 2, 255, 143, 9, 150, 143, 148, 3, 6, 255, 143, 9, 143, 3, 144, 6, 149, 255, 2, 5, 5, 150, 6, 148, 9, 148, 2, 9, 144, 145, 2, 1, 5, 242, 2, 2, 255, 9, 149, 255, 150, 143, 215, 2, 252, 9, 252, 255, 252, 255, 2, 3, 1, 242, 2, 242, 143, 3, 150, 0, 5, 2, 255, 144, 150, 0, 5, 3, 148, 5, 144, 145, 149, 2, 10, 3, 2, 148, 6, 2, 143, 0, 150, 145, 255, 4, 4, 4, 216, 54, 63>>, %{})
      {<<156, 187, 159, 2, 5, 3, 5, 242, 255, 4, 1, 6, 2, 255, 10, 242, 177, 242, 5, 145, 149, 4, 0, 5, 4, 4, 5, 148, 255, 149, 2, 144, 150, 2, 145, 2, 4, 5, 149, 150, 2, 3, 145, 6, 1, 9, 10, 9, 149, 6, 2, 0, 5, 144, 3, 9, 150, 1, 255, 9, 255, 2, 145, 0, 145, 10, 143, 5, 3, 150, 4, 144, 6, 255, 0, 5, 0, 0, 4, 3, 2, 3, 150, 9, 5, 4, 145, 2, 10, 0, 150, 1, 149, 9, 1, 144, 6, 150, 9, 4, 145, 3, 9, 255, 5, 4, 0, 150, 148, 9, 10, 148, 150, 2, 255, 143, 9, 150, 143, 148, 3, 6, 255, 143, 9, 143, 3, 144, 6, 149, 255, 2, 5, 5, 150, 6, 148, 9, 148, 2, 9, 144, 145, 2, 1, 5, 242, 2, 2, 255, 9, 149, 255, 150, 143, 215, 2, 252, 9, 252, 255, 252, 255, 2, 3, 1, 242, 2, 242, 143, 3, 150, 0, 5, 2, 255, 144, 150, 0, 5, 3, 148, 5, 144, 145, 149, 2, 10, 3, 2, 148, 6, 2, 143, 0, 150, 145, 255, 4, 4, 4>>, <<54, 63>>}

  """
  @spec next(binary()) :: {binary(), binary()}
  def next(raw) do
    raw
    |> :binary.split([<<0xD8>>])
    |> List.to_tuple()
  end

  @doc """
  Encrypt a login packet.

  ## Examples

      iex> LoginService.Endpoint.Cryptography.encrypt("fail Hello. This is a basic test")
      <<117, 112, 120, 123, 47, 87, 116, 123, 123, 126, 61, 47, 99, 119, 120, 130, 47, 120, 130, 47, 112, 47, 113, 112, 130, 120, 114, 47, 131, 116, 130, 131, 25>>
  """
  @spec encrypt(binary()) :: binary()
  def encrypt(packet) do
    <<encrypt_payload(packet)::binary, 0x19>>
  end

  defp encrypt_payload(payload) do
    for <<c <- payload>>, into: <<>> do
      <<band(c + 15, 0xFF)::8>>
    end
  end

  @doc """
  Decrypt a login packet.

  ## Examples

      iex> LoginService.Endpoint.Cryptography.decrypt(<<156, 187, 159, 2, 5, 3, 5, 242, 255, 4, 1, 6, 2, 255, 10, 242, 177, 242, 5, 145, 149, 4, 0, 5, 4, 4, 5, 148, 255, 149, 2, 144, 150, 2, 145, 2, 4, 5, 149, 150, 2, 3, 145, 6, 1, 9, 10, 9, 149, 6, 2, 0, 5, 144, 3, 9, 150, 1, 255, 9, 255, 2, 145, 0, 145, 10, 143, 5, 3, 150, 4, 144, 6, 255, 0, 5, 0, 0, 4, 3, 2, 3, 150, 9, 5, 4, 145, 2, 10, 0, 150, 1, 149, 9, 1, 144, 6, 150, 9, 4, 145, 3, 9, 255, 5, 4, 0, 150, 148, 9, 10, 148, 150, 2, 255, 143, 9, 150, 143, 148, 3, 6, 255, 143, 9, 143, 3, 144, 6, 149, 255, 2, 5, 5, 150, 6, 148, 9, 148, 2, 9, 144, 145, 2, 1, 5, 242, 2, 2, 255, 9, 149, 255, 150, 143, 215, 2, 252, 9, 252, 255, 252, 255, 2, 3, 1, 242, 2, 242, 143, 3, 150, 0, 5, 2, 255, 144, 150, 0, 5, 3, 148, 5, 144, 145, 149, 2, 10, 3, 2, 148, 6, 2, 143, 0, 150, 145, 255, 4, 4, 4>>, %{})
      "NoS0575 3614038 a 5AE625665F3E0BD0A065ED07A41989E4025B79D13930A2A8C57D6B4325226707D956A082D1E91B4D96A793562DF98FD03C9DCF743C9C7B4E3055D4F9F09BA015 0039E3DC\x0B0.9.3.3071 0 C7D2503BD257F5BAE0870F40C2DA3666"
  """
  @spec decrypt(binary) :: binary()
  def decrypt(<<>>) do
    <<>>
  end

  def decrypt(<<c>>) do
    case c do
      c when c > 0x0E -> <<bxor(c - 0x0F, 0xC3)::utf8>>
      c -> <<bxor(0xFF - (0x0E - c), 0xC3)::utf8>>
    end
  end

  def decrypt(binary, assigns) do
    for <<c <- binary>>, into: <<>>, do: decrypt(<<c>>, assigns)
  end

  @doc """
  Decrypt a password from the NoS0575 login packet.

  ## Examples

    iex> LoginService.Endpoint.Cryptography.decrypt_password("2EB6A196E4B60D96A9267E")
    "admin"
    iex> LoginService.Endpoint.Cryptography.decrypt_password("1BE97B527A306B597A2")
    "user"
  """
  @spec decrypt_password(String.t()) :: String.t()
  def decrypt_password(password) do
    password
    |> String.slice(
      case password |> String.length() |> rem(2) do
        0 -> 3..-1
        1 -> 4..-1
      end
    )
    |> String.codepoints()
    |> Enum.take_every(2)
    |> Enum.chunk_every(2)
    |> Enum.map(&Enum.join/1)
    |> Enum.map(&String.to_integer(&1, 16))
    |> to_string()
  end
end
