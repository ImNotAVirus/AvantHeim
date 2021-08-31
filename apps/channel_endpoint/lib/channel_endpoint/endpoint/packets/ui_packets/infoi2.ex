defmodule ChannelEndpoint.Endpoint.UIPackets.Infoi2 do
  @moduledoc """
  TODO: Documentation.

  Ex: infoi2 234 1 eazeza
  """

  use Core.SerializableStruct

  alias __MODULE__

  @enforce_keys [:i18n_vnum, :params_count, :name]
  defstruct @enforce_keys

  @type t :: %Infoi2{
          i18n_vnum: pos_integer,
          params_count: pos_integer,
          name: String.t()
        }

  @impl true
  def serialize(%Infoi2{} = struct, _) do
    %Infoi2{
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    } = struct

    ["infoi2", i18n_vnum, params_count, name]
  end
end
