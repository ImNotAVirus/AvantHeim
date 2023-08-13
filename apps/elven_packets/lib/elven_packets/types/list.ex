defmodule ElvenPackets.Types.NsList do
  @moduledoc """
  TODO: ElvenPackets.Types.NsList
  """

  use ElvenGard.Network.Type

  alias ElvenPackets.Types.NsString

  @type t :: list()

  ## Behaviour impls

  @impl true
  @spec decode(binary(), Keyword.t()) :: {t(), binary()}
  def decode(data, opts) when is_binary(data) do
    type = Keyword.get(opts, :type)
    joiner = Keyword.fetch!(opts, :joiner)

    {encoded, rest} =
      case joiner do
        " " -> {data, ""}
        _ -> NsString.decode(data)
      end

    encoded
    |> String.split(joiner)
    |> do_decode(type)
    |> then(&{&1, rest})
  end

  @impl true
  @spec encode(t(), Keyword.t()) :: iolist()
  def encode([], _opts), do: "-1"

  def encode(list, opts) when is_list(list) do
    type = Keyword.get(opts, :type)
    joiner = Keyword.fetch!(opts, :joiner)

    case {type, joiner} do
      {nil, " "} -> list
      {_, " "} -> Enum.map(list, &type.encode/1)
      {nil, _} -> Enum.join(list, joiner)
      _ -> list |> Enum.map(&type.encode/1) |> Enum.join(joiner)
    end
  end

  ## Helpers

  defp do_decode(encoded_list, nil), do: encoded_list
  defp do_decode(encoded_list, type), do: Enum.map(encoded_list, &do_decode_type(&1, type))

  defp do_decode_type(obj, type) do
    {result, ""} = type.decode(obj)
    result
  end
end
