defmodule Core.Socket.Serializer do
  @moduledoc """
  TODO: Documentation
  """

  @doc """
  Converts the argument to an iodata according to the
  `Core.Socket.SerializerProtocol` protocol.
  This is the function invoked when a sezializer have to
  serialize a term.
  """
  defmacro serialize_term(term, opts \\ []) do
    quote do
      :"Elixir.Core.Socket.SerializerProtocol".serialize(unquote(term), unquote(opts))
    end
  end
end
