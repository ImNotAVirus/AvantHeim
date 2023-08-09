defmodule LoginService.Types.NsInteger do
  @moduledoc """
  TODO: LoginService.Types.NsInteger
  """

  use ElvenGard.Network.Type

  @type t :: integer()

  @sep [" ", "\v"]

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    case String.split(data, @sep, parts: 2) do
      [string] -> {String.to_integer(string, 10), ""}
      [string, rest] -> {String.to_integer(string, 10), rest}
    end
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_binary(data) do
    Integer.to_string(data)
  end
end
