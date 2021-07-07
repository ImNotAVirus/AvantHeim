defmodule ChannelEndpoint.Endpoint.PlayerPackets.Fs do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct
  
  import DatabaseService.PlayerEnums, only: [faction: 1]

  alias __MODULE__

  @enforce_keys [:faction]
  defstruct @enforce_keys

  @type t :: %Fs{faction: atom}

  @impl true
  def serialize(%Fs{} = struct, _) do
    ["fs", faction(struct.faction)]
  end
end
