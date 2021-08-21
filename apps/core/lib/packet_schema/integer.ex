defmodule Core.PacketSchema.Integer do
  @moduledoc """
  TODO: Documentation
  """

  @spec parse(String.t(), keyword) :: {:ok, value :: integer, rest :: any}
  def parse(bin, opts) do
    sep = Keyword.fetch!(opts, :separator)

    case String.split(bin, sep, parts: 2) do
      [value, rest] -> {:ok, String.to_integer(value), rest}
      [value] -> {:ok, String.to_integer(value), ""}
    end
  end
end
