defmodule Core.PacketSchema.String do
  @moduledoc """
  TODO: Documentation
  """

  @spec parse(String.t(), keyword) :: {:ok, value :: String.t(), rest :: any}
  def parse(bin, opts) do
    fill = Keyword.get(opts, :fill)
    sep = Keyword.fetch!(opts, :separator)

    if fill == true do
      {:ok, bin, ""}
    else
      case String.split(bin, sep, parts: 2) do
        [value, rest] -> {:ok, value, rest}
        [value] -> {:ok, value, ""}
      end
    end
  end
end
