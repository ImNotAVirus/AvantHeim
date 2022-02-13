defmodule ElvenCore.PacketSchema.List do
  @moduledoc """
  TODO: Documentation
  """

  ## Public API

  @spec parse(String.t(), keyword) :: {:ok, value :: [any], rest :: String.t()} | :error
  def parse(bin, opts) do
    {type, tmp_opts} = Keyword.pop!(opts, :type)
    {size, type_opts} = Keyword.pop!(tmp_opts, :size)

    case size do
      :fill ->
        decode_fill(bin, type, type_opts)

      l when is_atom(l) ->
        size_from_field = get_in(type_opts, [:fields, l])
        decode_with_size(bin, type, size_from_field, type_opts)

      l when is_integer(l) ->
        decode_with_size(bin, type, size, type_opts)
    end
  end

  ## Private functions

  defp decode_fill(bin, type_mod, type_opts, result \\ [])
  defp decode_fill(<<>>, _, _, result), do: Enum.reverse(result)

  defp decode_fill(bin, type_mod, type_opts, result) do
    case apply(type_mod, :parse, [bin, type_opts]) do
      {:ok, value, rest} -> decode_fill(rest, type_mod, type_opts, [value | result])
      _ -> :error
    end
  end

  defp decode_with_size(bin, type_mod, size, type_opts, result \\ [])
  defp decode_with_size(bin, _, 0, _, result), do: {:ok, Enum.reverse(result), bin}
  defp decode_with_size(<<>>, _, _, _, _), do: :error
  # defp decode_with_size(<<>>, _, size, _, _) when not is_integer(size), do: :error

  defp decode_with_size(bin, type_mod, size, type_opts, result) do
    case apply(type_mod, :parse, [bin, type_opts]) do
      {:ok, value, rest} ->
        decode_with_size(rest, type_mod, size - 1, type_opts, [value | result])

      _ ->
        :error
    end
  end
end
