defmodule ChannelEndpoint.Endpoint.UIPackets.Msgi do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import ChannelEndpoint.Endpoint.UIPackets.MessageTypeEnum, only: [message_type: 2]

  alias __MODULE__

  @enforce_keys [:message_type, :i18n_vnum]
  defstruct @enforce_keys ++
              [
                first_argument: 0,
                second_argument: 0,
                third_argument: 0,
                fourth_argument: 0,
                fifth_argument: 0
              ]

  @type t :: %Msgi{
          message_type: MessageTypeEnum.message_type_keys(),
          i18n_vnum: pos_integer()
        }

  @impl true
  def serialize(%Msgi{} = struct, _) do
    %Msgi{
      message_type: message_type_atom,
      i18n_vnum: i18n_vnum,
      first_argument: first_argument,
      second_argument: second_argument,
      third_argument: third_argument,
      fourth_argument: fourth_argument,
      fifth_argument: fifth_argument
    } = struct

    [
      "msgi",
      message_type(message_type_atom, :value),
      i18n_vnum,
      first_argument,
      second_argument,
      third_argument,
      fourth_argument,
      fifth_argument
    ]
  end
end
