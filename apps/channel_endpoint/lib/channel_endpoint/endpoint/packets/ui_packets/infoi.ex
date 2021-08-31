defmodule ChannelEndpoint.Endpoint.UIPackets.Infoi do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  require DatabaseService.PlayerEnums

  alias __MODULE__

  @enforce_keys [:i18n_vnum]
  defstruct @enforce_keys ++ [first_argument: 0, second_argument: 0, third_argument: 0]

  @type t :: %Infoi{
        i18n_vnum: pos_integer()
        }

  @impl true
  def serialize(%Infoi{} = struct, _) do
    %Infoi{
      i18n_vnum: i18n_vnum,
      first_argument: first_argument,
      second_argument: second_argument,
      third_argument: third_argument
    } = struct

    ["infoi", i18n_vnum, first_argument, second_argument, third_argument]
  end
end
