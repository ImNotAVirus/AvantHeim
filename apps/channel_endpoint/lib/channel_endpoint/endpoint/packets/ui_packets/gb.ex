defmodule ChannelEndpoint.Endpoint.UIPackets.Gb do
  @moduledoc """
  TODO: Documentation
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.UIPackets.BankActionTypesEnums, only: [bank_action_type: 1]

  alias __MODULE__

  @enforce_keys [:bank_action_type, :gold_bank, :gold, :bank_rank, :bank_tax]
  defstruct @enforce_keys

  @type t :: %Gb{
          bank_action_type: atom,
          gold_bank: non_neg_integer,
          gold: non_neg_integer,
          bank_rank: pos_integer,
          bank_tax: non_neg_integer
        }

  @impl true
  def serialize(%Gb{} = struct, _) do
    %Gb{
      bank_action_type: bank_action_type_atom,
      gold_bank: gold_bank,
      gold: gold,
      bank_rank: bank_rank,
      bank_tax: bank_tax
    } = struct

    ["gb", bank_action_type(bank_action_type_atom), gold_bank, gold, bank_rank, bank_tax]
  end
end
