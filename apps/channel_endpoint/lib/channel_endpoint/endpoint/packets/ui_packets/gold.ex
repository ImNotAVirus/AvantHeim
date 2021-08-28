defmodule ChannelEndpoint.Endpoint.UIPackets.Gold do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:character_gold, :character_bank_gold]
  defstruct @enforce_keys

  @type t :: %Gold{
    character_gold: non_neg_integer,
    character_bank_gold: non_neg_integer
  }

  @impl true
  def serialize(%Gold{} = struct, _) do
    %Gold{character_gold: character_gold, character_bank_gold: character_bank_gold} = struct
    ["gold", character_gold, character_bank_gold]
  end
end
