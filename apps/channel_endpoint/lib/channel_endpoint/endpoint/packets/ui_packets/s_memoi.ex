defmodule ChannelEndpoint.Endpoint.UIPackets.SMemoi do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.UIPackets.BankTextEnums, only: [text_type: 2]

  alias __MODULE__

  @enforce_keys [:text_type, :i18n_string]
  defstruct @enforce_keys

  @type t :: %SMemoi{
          text_type: atom,
          i18n_string: pos_integer
        }

  @impl true
  def serialize(%SMemoi{} = struct, _) do
    %SMemoi{
      text_type: text_type_atom,
      i18n_string: i18n_string
    } = struct

    ["s_memoi", text_type(text_type_atom, :value), i18n_string, 0]
  end
end
