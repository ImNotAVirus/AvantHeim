defmodule LoginService.Types.NsString do
  @moduledoc """
  A sequence of Unicode scalar values
  """

  use ElvenGard.Network.Type

  @type t :: String.t()

  @sep [" ", "\v"]

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    case String.split(data, @sep, parts: 2) do
      [string] -> {string, ""}
      [string, rest] -> {string, rest}
    end
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_binary(data) do
    data
  end
end
