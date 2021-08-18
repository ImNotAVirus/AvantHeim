defmodule Core.Socket.Serializers do
  @moduledoc false

  import Core.Socket.Serializer, only: [serialize_term: 2]

  defimpl Core.Socket.SerializerProtocol, for: Integer do
    def serialize(data, _opts), do: Integer.to_string(data)
  end

  defimpl Core.Socket.SerializerProtocol, for: BitString do
    def serialize(data, opts) do
      escape = Keyword.get(opts, :escape, false)

      case {data, escape} do
        {"", _} -> "-"
        {x, false} -> x
        {x, _} -> String.replace(x, " ", "^")
      end
    end
  end

  defimpl Core.Socket.SerializerProtocol, for: Atom do
    def serialize(data, opts) do
      {as, _new_opts} = Keyword.pop(opts, :as)

      case {data, as} do
        {nil, :string} ->
          "-"

        {nil, :integer} ->
          "-1"

        {true, _} ->
          "1"

        {false, _} ->
          "0"

        {x, _} ->
          raise "can't serialize atom: #{inspect(x)}. " <>
                  "Try to use :as options or call serialize_term/2"
      end
    end
  end

  defimpl Core.Socket.SerializerProtocol, for: List do
    def serialize([], _), do: "-1"

    def serialize(data, opts) do
      {joiner, new_opts} = Keyword.pop(opts, :joiner, ".")

      data
      |> Enum.map(&serialize_term(&1, new_opts))
      |> Enum.join(joiner)
    end
  end
end
