defmodule ChannelService.Endpoint.UIPackets.SMemoi do
  @moduledoc """
  TODO: Documentation.
  """

  use ElvenCore.SerializableStruct

  import ElvenEnums.BankEnums, only: [text_color: 2]

  alias __MODULE__

  @enforce_keys [:text_color, :i18n_vnum]
  defstruct @enforce_keys

  @type t :: %SMemoi{text_color: atom, i18n_vnum: pos_integer}

  @impl true
  def serialize(%SMemoi{} = struct, _) do
    %SMemoi{
      text_color: text_color_atom,
      i18n_vnum: i18n_vnum
    } = struct

    ["s_memoi", text_color(text_color_atom, :value), i18n_vnum, 0]
  end
end
