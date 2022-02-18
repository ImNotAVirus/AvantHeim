defmodule ChannelService.Endpoint.LobbyPackets.ClistStart do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  alias __MODULE__

  defstruct []

  @type t :: %ClistStart{}

  @impl true
  def serialize(%ClistStart{}, _), do: "clist_start 0"
end
