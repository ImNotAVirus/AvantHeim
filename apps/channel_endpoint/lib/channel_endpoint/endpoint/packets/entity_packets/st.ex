defmodule ChannelEndpoint.Endpoint.EntityPackets.St do
  @moduledoc """
  TODO: Documentation.
  """

  use Core.SerializableStruct

  import DatabaseService.EntityEnums, only: [entity_type: 2]

  alias __MODULE__

  @enforce_keys [
    :entity_type,
    :entity_id,
    :level,
    :hp,
    :hp_max,
    :mp,
    :mp_max,
    :buffs
  ]

  @optional_keys [hero_level: 0]

  defstruct @enforce_keys ++ @optional_keys

  @type t :: %St{
          entity_type: atom,
          entity_id: pos_integer,
          level: pos_integer,
          hero_level: non_neg_integer,
          hp: non_neg_integer,
          hp_max: non_neg_integer,
          mp: non_neg_integer,
          mp_max: non_neg_integer,
          buffs: [pos_integer]
        }

  @impl true
  def serialize(%St{} = struct, _) do
    %St{
      entity_type: entity_type_atom,
      entity_id: entity_id,
      level: level,
      hero_level: hero_level,
      hp: hp,
      hp_max: hp_max,
      mp: mp,
      mp_max: mp_max,
      buffs: buffs
    } = struct

    hp_percent = trunc(hp * 100 / hp_max)
    mp_percent = trunc(mp * 100 / mp_max)

    packet = [
      "st",
      entity_type(entity_type_atom, :value),
      entity_id,
      level,
      hero_level,
      hp_percent,
      mp_percent,
      hp,
      mp
    ]

    case buffs do
      [] -> packet
      _ -> packet ++ serialize_term(buffs, joiner: " ")
    end
  end
end
