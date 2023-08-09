defmodule ElvenI18n.PacketConstString do
  @moduledoc """
  TODO: Documentation
  """

  @priv_dir :code.priv_dir(:elven_i18n)
  @path Path.join(@priv_dir, "Game18NConstString.yaml")
  @external_resource @path

  def new!(key, args \\ []) do
    case new(key, args) do
      {:ok, result} -> result
      {:error, error} -> raise "#{inspect(error)}"
    end
  end

  def new(key, args \\ [])

  def_getter = fn state ->
    key = Map.fetch!(state, "key")
    value = Map.fetch!(state, "value")
    args = Map.fetch!(state, "args")

    quote unquote: true do
      def new(unquote(key), args) do
        case check_args(args, unquote(args)) do
          false -> {:error, :badargs}
          true -> {:ok, %{key: unquote(key), value: unquote(value), args: args}}
        end
      end
    end
  end

  @path
  |> YamlElixir.read_all_from_file!()
  |> Enum.map(def_getter)
  |> Code.eval_quoted([], __ENV__)

  ## Private helpers

  defp check_args(args, types) when length(args) != length(types), do: false

  defp check_args(args, types) do
    check_type = fn
      {arg, "string"} -> is_binary(arg)
      {arg, "integer"} -> is_integer(arg)
    end

    args
    |> Enum.zip(types)
    |> Enum.map(check_type)
    |> Enum.all?()
  end
end
