defmodule ChannelEndpoint.Endpoint.LobbyPackets.Ok do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %Ok{}

  @impl true
  def serialize(%Ok{}, _), do: "OK"
end
