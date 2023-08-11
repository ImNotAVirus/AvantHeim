defmodule ElvenPackets.Types.NsInteger do
  @moduledoc """
  TODO: ElvenPackets.Types.NsInteger
  """

  use ElvenGard.Network.Type

  @type t :: integer()

  ## Behaviour impls

  @impl true
  @spec decode(binary(), Keyword.t()) :: {t(), binary()}
  def decode(data, _opts) when is_binary(data) do
    case String.split(data, ElvenPackets.separator(), parts: 2) do
      [string] -> {String.to_integer(string, 10), ""}
      [string, rest] -> {String.to_integer(string, 10), rest}
    end
  end

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_integer(data) do
    Integer.to_string(data)
  end
end
