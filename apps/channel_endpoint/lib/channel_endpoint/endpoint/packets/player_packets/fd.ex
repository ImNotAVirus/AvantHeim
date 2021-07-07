defmodule ChannelEndpoint.Endpoint.PlayerPackets.Fd do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct
  
  alias __MODULE__

  @enforce_keys [:reputation, :reputation_icon_id, :dignity, :dignity_icon_id]
  defstruct @enforce_keys

  @type t :: %Fd{
    reputation: integer,
    reputation_icon_id: integer,
    dignity: integer,
    dignity_icon_id: integer,
  }

  @impl true
  def serialize(%Fd{} = struct, _) do
    %Fd{
      reputation: reputation,
      reputation_icon_id: reputation_icon_id,
      dignity: dignity,
      dignity_icon_id: dignity_icon_id,
    } = struct
    
    ["fd", reputation, reputation_icon_id, dignity, dignity_icon_id]
  end
end
