defmodule ChannelService.Endpoint.UIPackets.Gb do
  @moduledoc """
  TODO: Documentation
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.BankEnums, only: [action_type: 2]

  alias __MODULE__

  @enforce_keys [:action_type, :bank_gold, :gold, :bank_rank, :bank_tax]
  defstruct @enforce_keys

  @type t :: %Gb{
          action_type: atom,
          bank_gold: non_neg_integer,
          gold: non_neg_integer,
          bank_rank: pos_integer,
          bank_tax: non_neg_integer
        }

  @impl true
  def serialize(%Gb{} = struct, _) do
    %Gb{
      action_type: action_type_atom,
      bank_gold: bank_gold,
      gold: gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    } = struct

    ["gb", action_type(action_type_atom, :value), bank_gold, gold, bank_rank, bank_tax]
  end
end
