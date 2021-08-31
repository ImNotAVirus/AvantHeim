defmodule ChannelEndpoint.Endpoint.UIPackets.Dlgi2 do
  @moduledoc """
  TODO: Documentation.

  Ex: dlgi2 #pjoin^3^1181905 #pjoin^4^1181905 233 1 Living
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:packet_yes, :packet_no, :i18n_vnum, :params_count, :name]
  defstruct @enforce_keys

  @type t :: %Dlgi2{
          packet_yes: struct,
          packet_no: struct,
          i18n_vnum: pos_integer,
          name: String.t()
        }

  @impl true
  def serialize(%Dlgi2{} = struct, _) do
    %Dlgi2{
      packet_yes: packet_yes,
      packet_no: packet_no,
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    } = struct

    ["dlgi2", packet_to_field(packet_yes), packet_to_field(packet_no), i18n_vnum, params_count, name]
  end

  ## Private function

  defp packet_to_field(packet) do
    String.replace("##{serialize_term(packet)}", " ", "^")
  end
end
