defmodule ChannelEndpoint.Endpoint.MapPackets.MapOut do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %MapOut{}

  @impl true
  @spec serialize(MapOut.t(), any) :: String.t()
  def serialize(%MapOut{}, _), do: "mapout"
end
