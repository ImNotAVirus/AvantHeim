defmodule ChannelEndpoint.Endpoint.EntityPacket.Pinit.SubGroupMember do
  alias DatabaseService.EntityEnums
  alias DatabaseService.PlayerEnums

  alias __MODULE__

  import DatabaseService.EntityEnums, only: [entity_type: 2]
  import DatabaseService.PlayerEnums, only: [gender: 2]

  use Core.SerializableStruct

  @enforce_keys [
    :entity_type,
    :entity_id,
    :group_position,
    :level,
    :name,
    :unknow,
    :gender,
    :race,
    :morph,
    :hero_level,
    :unknow1,
    :unknow2
  ]
  defstruct @enforce_keys

  @type t :: %SubGroupMember{
          entity_type: EntityEnums.visual_type_keys(),
          entity_id: pos_integer,
          group_position: non_neg_integer,
          level: pos_integer,
          name: String.t(),
          unknow: non_neg_integer,
          gender: PlayerEnums.gender_keys(),
          race: non_neg_integer,
          morph: pos_integer,
          hero_level: pos_integer,
          unknow1: non_neg_integer,
          unknow2: non_neg_integer
        }

  ## Public API

  @impl true
  def serialize(%SubGroupMember{} = struct, _) do
    %SubGroupMember{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      group_position: group_position,
      level: level,
      name: name,
      unknow: unknow,
      gender: gender,
      race: race,
      morph: morph,
      hero_level: hero_level,
      unknow1: unknow1,
      unknow2: unknow2
    } = struct

    serialize_term(
      [
        entity_type(entity_type_atom, :value),
        entity_id,
        group_position,
        level,
        name,
        unknow,
        gender(gender, :value),
        race,
        morph,
        hero_level,
        unknow1,
        unknow2
      ],
      joiner: "|"
    )
  end
end
