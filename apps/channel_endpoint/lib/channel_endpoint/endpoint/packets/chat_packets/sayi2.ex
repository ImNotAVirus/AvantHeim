defmodule ChannelEndpoint.Endpoint.ChatPackets.Sayi2 do
  @moduledoc """
  TODO: Documentation.

  Ex: sayi2 1 1207248 10 237 1 eazeza
  """

  use Core.SerializableStruct

  alias DatabaseService.EntityEnums
  alias ChannelEndpoint.Endpoint.ChatPackets.SayEnums

  import DatabaseService.EntityEnums, only: [entity_type: 2]
  import ChannelEndpoint.Endpoint.ChatPackets.SayEnums, only: [color_type: 2]

  alias __MODULE__

  @enforce_keys [:entity_type, :entity_id, :color, :i18n_vnum, :params_count, :name]
  defstruct @enforce_keys

  @type t :: %Sayi2{
          entity_type: EntityEnums.entity_type_keys(),
          entity_id: pos_integer,
          color: SayEnums.color_type_keys(),
          i18n_vnum: pos_integer,
          params_count: pos_integer,
          name: String.t()
        }

  @impl true
  def serialize(%Sayi2{} = struct, _) do
    %Sayi2{
      entity_type: entity_type,
      entity_id: entity_id,
      color: color,
      i18n_vnum: i18n_vnum,
      params_count: params_count,
      name: name
    } = struct

    [
      "sayi2",
      entity_type(entity_type, :value),
      entity_id,
      color_type(color, :value),
      i18n_vnum,
      params_count,
      name
    ]
  end
end
