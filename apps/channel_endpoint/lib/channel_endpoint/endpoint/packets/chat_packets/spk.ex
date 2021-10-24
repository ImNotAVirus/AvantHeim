defmodule ChannelEndpoint.Endpoint.ChatPackets.Spk do
  @moduledoc """
  TODO: Documentation.

  Ex: spk 1 2 3 Name test
  """

  use Core.SerializableStruct

  alias DatabaseService.EntityEnums
  alias ChannelEndpoint.Endpoint.ChatPackets.SpeakEnums

  import DatabaseService.EntityEnums, only: [entity_type: 2]
  import ChannelEndpoint.Endpoint.ChatPackets.SpeakEnums, only: [speak_type: 2]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :speak_type, :entity_name, :message]
  defstruct @enforce_keys

  @type t :: %Spk{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          speak_type: SpeakEnums.speak_type_keys(),
          entity_name: String.t(),
          message: String.t()
        }

  @impl true
  def serialize(%Spk{} = struct, _) do
    %Spk{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      speak_type: speak_type_atom,
      entity_name: entity_name,
      message: message
    } = struct

    packet = [
      "spk",
      entity_type(entity_type_atom, :value),
      entity_id,
      speak_type(speak_type_atom, :value)
    ]

    case {entity_name, message} do
      {nil, nil} -> packet

      {nil, _} -> packet ++ message

      {_, nil} -> packet ++ entity_name

      {_, _} -> packet ++ entity_name ++ message
    end
  end
end
