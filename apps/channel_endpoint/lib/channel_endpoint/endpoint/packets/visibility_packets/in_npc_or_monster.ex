defmodule ChannelEndpoint.Endpoint.VisibilityPackets.InNpcOrMonster do
  @moduledoc """
  TODO: Documentation.

  in 2 1946 2376 75 120 2 100 100 468 0 0 -1 1 0 -1 - 0 -1 0 0 0 0 0 0 0 0 0 0 0
  in 3 333 2319 75 120 2 100 100 0 0 0 -1 1 0 -1 - 0 -1 0 0 0 0 0 0 0 0 0 0 0

  out 3 2319
  in 3 333 2319 75 120 2 100 100 0 0 0 -1 2 1 -1 Nameeee 0 -1 0 0 0 1 0 0 0 0 0 0 0
  """

  use Core.SerializableStruct

  require ChannelEndpoint.MapEnums
  require DatabaseService.EntityEnums

  alias __MODULE__
  alias ChannelEndpoint.MapEnums
  alias DatabaseService.EntityEnums

  @enforce_keys [
    :entity_type,
    :vnum,
    :id,
    :map_x,
    :map_y,
    :direction,
    :hp_percent,
    :mp_percent,
    :is_sitting,
    :is_invisible
  ]

  @optional_keys [
    dialog: 0,
    spawn_effect: :no_effect,
    name: nil
  ]

  defstruct @enforce_keys ++ @optional_keys

  @type t :: %InNpcOrMonster{
          entity_type: EntityEnums.entity_type_keys(),
          vnum: pos_integer,
          id: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          direction: EntityEnums.direction_type_keys(),
          hp_percent: non_neg_integer,
          mp_percent: non_neg_integer,
          dialog: non_neg_integer,
          spawn_effect: MapEnums.spawn_effect_type_keys(),
          is_sitting: boolean,
          is_invisible: boolean
        }

  @impl true
  def serialize(%InNpcOrMonster{} = struct, _) do
    %InNpcOrMonster{
      entity_type: entity_type,
      vnum: vnum,
      id: id,
      map_x: map_x,
      map_y: map_y,
      direction: direction,
      hp_percent: hp_percent,
      mp_percent: mp_percent,
      dialog: dialog,
      spawn_effect: spawn_effect,
      is_sitting: is_sitting,
      name: name,
      is_invisible: is_invisible
    } = struct

    unknown1 = 0
    unknown2 = 0
    unknown3 = -1
    unknown4 = -1
    unknown5 = 0
    unknown6 = -1
    unknown7 = 0
    unknown8 = 0
    unknown9 = 0
    unknown10 = 0
    unknown11 = 0
    unknown12 = 0
    unknown13 = 0
    unknown14 = 0
    unknown15 = 0
    unknown16 = 0

    [
      "in",
      EntityEnums.entity_type(entity_type, :value),
      vnum,
      id,
      map_x,
      map_y,
      EntityEnums.direction_type(direction, :value),
      hp_percent,
      mp_percent,
      dialog,
      unknown1,
      unknown2,
      unknown3,
      MapEnums.spawn_effect_type(spawn_effect, :value),
      is_sitting,
      unknown4,
      serialize_term(name, as: :string),
      unknown5,
      unknown6,
      unknown7,
      unknown8,
      unknown9,
      unknown10,
      unknown11,
      unknown12,
      unknown13,
      is_invisible,
      unknown14,
      unknown15,
      unknown16
    ]
  end
end
