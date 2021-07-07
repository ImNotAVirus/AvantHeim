defmodule ChannelEndpoint.Endpoint.LobbyPackets.ClistEnd do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %ClistEnd{}

  @impl true
  def serialize(%ClistEnd{}, _), do: "clist_end"
end
