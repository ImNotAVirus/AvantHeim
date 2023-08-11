defmodule ElvenPackets.Types.NsBoolean do
  @moduledoc """
  TODO: ElvenPackets.Types.NsBoolean
  """

  use ElvenGard.Network.Type

  @type t :: boolean()

  ## Behaviour impls

  @impl true
  @spec decode(binary(), Keyword.t()) :: {t(), binary()}
  def decode(data, _opts) when is_binary(data) do
    case String.split(data, ElvenPackets.separator(), parts: 2) do
      [value] -> {to_bool(value), ""}
      [value, rest] -> {to_bool(value), rest}
    end
  end

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(true, _opts), do: "1"
  def encode(false, _opts), do: "0"

  ## Helpers

  defp to_bool("1"), do: true
  defp to_bool("0"), do: false
end
