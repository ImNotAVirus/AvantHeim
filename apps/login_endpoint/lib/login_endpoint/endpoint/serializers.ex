defmodule LoginEndpoint.Endpoint.Serializers do
  @moduledoc false

  import Core.Socket.Serializer, only: [serialize_term: 2]

  defimpl Core.Socket.SerializerProtocol, for: BitString do
    def serialize(data, _opts), do: data
  end

  defimpl Core.Socket.SerializerProtocol, for: Integer do
    def serialize(data, _opts), do: Integer.to_string(data)
  end

  defimpl Core.Socket.SerializerProtocol, for: List do
    def serialize(data, opts) do
      {joiner, new_opts} = Keyword.pop(opts, :joiner, " ")

      data
      |> Enum.map(&serialize_term(&1, new_opts))
      |> Enum.join(joiner)
    end
  end
end
