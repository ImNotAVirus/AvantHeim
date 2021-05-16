defmodule ChannelEndpoint.Endpoint.ClistStartPacket do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %ClistStartPacket{}

  @impl true
  def serialize(%ClistStartPacket{}, _), do: "clist_start 0"
end
