defmodule ChannelEndpoint.Endpoint.UIPackets.Info do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:message]
  defstruct @enforce_keys

  @type t :: %Info{message: String.t()}

  @impl true
  def serialize(%Info{} = struct, _) do
    ["info", struct.message]
  end
end
