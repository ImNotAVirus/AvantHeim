defmodule ElvenViews.SerializableEnum do
  @moduledoc """
  TODO: Documentation.
  """

  @enforce_keys [:enumerators]
  defstruct @enforce_keys

  alias __MODULE__

  @type t :: %SerializableEnum{enumerators: Keyword.t()}

  ## Public API

  @spec new(Keyword.t()) :: t()
  def new(enumerators) when is_list(enumerators) do
    %SerializableEnum{enumerators: enumerators}
  end

  ## Implement serializable protocol

  defimpl ElvenCore.Socket.SerializerProtocol do
    def serialize(%SerializableEnum{enumerators: enum}, for: key) do
      enum[key] || raise "key not found in #{inspect(enum)}"
    end
  end
end
