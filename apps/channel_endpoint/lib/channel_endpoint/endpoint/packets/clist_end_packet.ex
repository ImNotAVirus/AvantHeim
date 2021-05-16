defmodule ChannelEndpoint.Endpoint.ClistEndPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %ClistEndPacket{}

  @impl true
  def serialize(%ClistEndPacket{}, _), do: "clist_end"
end
