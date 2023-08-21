defmodule ElvenCore.Socket.Serializers do
  @moduledoc false

  import ElvenCore.Socket.Serializer, only: [serialize_term: 2]

  defimpl ElvenCore.Socket.SerializerProtocol, for: Integer do
    def serialize(data, _opts), do: Integer.to_string(data)
  end

  defimpl ElvenCore.Socket.SerializerProtocol, for: BitString do
    def serialize(data, opts) do
      escape = Keyword.get(opts, :escape, false)

      case {data, escape} do
        {"", _} -> "-"
        {x, false} -> x
        {x, _} -> String.replace(x, " ", "^")
      end
    end
  end

  defimpl ElvenCore.Socket.SerializerProtocol, for: Atom do
    def serialize(data, opts) do
      as = Keyword.get(opts, :as)

      raise_fun = fn value ->
        raise "can't serialize atom: #{inspect(value)}. " <>
                "Try to use :as options or call serialize_term/2"
      end

      case {data, as} do
        {:"$drop", _} -> :"$drop"
        {nil, :string} -> "-"
        {nil, :integer} -> "-1"
        {nil, :drop} -> :"$drop"
        {true, _} -> "1"
        {false, _} -> "0"
        {x, _} -> raise_fun.(x)
      end
    end
  end

  defimpl ElvenCore.Socket.SerializerProtocol, for: List do
    def serialize([], _), do: "-1"

    def serialize(data, opts) do
      {joiner, new_opts} = Keyword.pop(opts, :joiner, ".")

      data
      |> Enum.map(&serialize_term(&1, new_opts))
      |> Enum.reject(&(&1 == :"$drop"))
      |> Enum.join(joiner)
    end
  end
end
