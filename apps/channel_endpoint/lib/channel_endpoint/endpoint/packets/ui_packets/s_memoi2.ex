defmodule ChannelEndpoint.Endpoint.UIPackets.SMemoi2 do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.UIPackets.BankTextEnums, only: [text_type: 2]

  alias __MODULE__

  @enforce_keys [:text_type, :i18n_string, :gold_bank, :gold]
  defstruct @enforce_keys

  @type t :: %SMemoi2{
          text_type: atom,
          # TODO : atom
          i18n_string: pos_integer,
          gold_bank: non_neg_integer,
          gold: non_neg_integer
        }

  @impl true
  def serialize(%SMemoi2{} = struct, _) do
    %SMemoi2{
      text_type: text_type_atom,
      i18n_string: i18n_string,
      gold_bank: gold_bank,
      gold: gold
    } = struct

    ["s_memoi2", text_type(text_type_atom, :value), i18n_string, 3, gold_bank, gold, 0]
  end
end
