defmodule ChannelService.Endpoint.UIPackets.SMemoi2 do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenViews.UIPackets.BankEnums, only: [text_color: 2]

  alias __MODULE__

  @enforce_keys [:text_color, :i18n_vnum, :bank_gold, :gold]
  defstruct @enforce_keys

  @type t :: %SMemoi2{
          text_color: atom,
          # TODO : atom
          i18n_vnum: pos_integer,
          bank_gold: non_neg_integer,
          gold: non_neg_integer
        }

  @impl true
  def serialize(%SMemoi2{} = struct, _) do
    %SMemoi2{
      text_color: text_color_atom,
      i18n_vnum: i18n_vnum,
      bank_gold: bank_gold,
      gold: gold
    } = struct

    display_bank_gold = round(bank_gold / 1000)

    [
      "s_memoi2",
      text_color(text_color_atom, :value),
      i18n_vnum,
      3,
      ElvenCore.format_number(display_bank_gold),
      ElvenCore.format_number(gold),
      0
    ]
  end
end
