defmodule ChannelEndpoint.Endpoint.LobbyPackets.ClistStart do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %ClistStart{}

  @impl true
  def serialize(%ClistStart{}, _), do: "clist_start 0"
end
