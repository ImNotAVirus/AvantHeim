defmodule ChannelEndpoint.Endpoint.UIPackets.Gold do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:gold, :bank_gold]
  defstruct @enforce_keys

  @type t :: %Gold{
          gold: non_neg_integer,
          bank_gold: non_neg_integer
        }

  @impl true
  def serialize(%Gold{} = struct, _) do
    %Gold{gold: gold, bank_gold: bank_gold} = struct
    ["gold", gold, bank_gold]
  end
end
