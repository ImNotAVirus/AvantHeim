defmodule ChannelEndpoint.Endpoint.PlayerPackets.Stat do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:hp, :hp_max, :mp, :mp_max]
  defstruct @enforce_keys

  @type t :: %Stat{
    hp: non_neg_integer,
    hp_max: non_neg_integer,
    mp: non_neg_integer,
    mp_max: non_neg_integer,
  }

  @impl true
  def serialize(%Stat{} = struct, _) do
    %Stat{hp: hp, hp_max: hp_max, mp: mp, mp_max: mp_max} = struct
    
    # Rainbow smiley
    options = 1056
    
    "stat #{hp} #{hp_max} #{mp} #{mp_max} 0 #{options}"
  end
end
