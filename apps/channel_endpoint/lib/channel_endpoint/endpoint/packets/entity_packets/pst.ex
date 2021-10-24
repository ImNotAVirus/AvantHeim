defmodule ChannelEndpoint.Endpoint.EntityPackets.Pst do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  alias __MODULE__

  import DatabaseService.EntityEnums, only: [entity_type: 2]
  import DatabaseService.PlayerEnums, only: [gender: 2]

  @enforce_keys [
    :entity_type,
    :entity_id,
    :group_order,
    :hp_left,
    :mp_left,
    :hp_load,
    :mp_load,
    :race,
    :gender,
    :morph,
    :buff_ids
  ]
  defstruct @enforce_keys

  @type t :: %Pst{
          entity_type: EntityEnums.visual_type_keys(),
          entity_id: pos_integer,
          group_order: non_neg_integer,
          hp_left: pos_integer,
          mp_left: pos_integer,
          hp_load: pos_integer,
          mp_load: pos_integer,
          race: non_neg_integer,
          gender: PlayerEnums.gender_keys(),
          morph: pos_integer,
          buff_ids: list
        }

  @impl true
  def serialize(%Pst{} = struct, _) do
    %Pst{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      group_order: group_order,
      hp_left: hp_left,
      mp_left: mp_left,
      hp_load: hp_load,
      mp_load: mp_load,
      race: race,
      gender: gender_atom,
      morph: morph,
      buff_ids: buff_ids
    } = struct

    [
      "pst",
      entity_type(entity_type_atom, :value),
      entity_id,
      group_order,
      hp_left,
      mp_left,
      hp_load,
      mp_load,
      race,
      gender(gender_atom, :value),
      morph,
      buff_ids
    ]
  end
end
