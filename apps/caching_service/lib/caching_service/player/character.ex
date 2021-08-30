defmodule CachingService.Player.Character do
  @moduledoc """
  TODO: Documentation
  """

  alias Core.Socket
  alias CachingService.Position
  alias DatabaseService.EntityEnums

  @db_attributes [
    :id,
    :name,
    :gender,
    :class,
    :hair_color,
    :hair_style,
    :faction,
    :map_vnum,
    :map_x,
    :map_y,
    :level,
    :job_level,
    :hero_level,
    :level_xp,
    :job_level_xp,
    :hero_level_xp,
    :gold,
    :bank_gold
  ]

  @virtual_attributes %{
    socket: nil,
    map_id: nil,
    speed: 20,
    direction: :south
  }

  use Memento.Table,
    type: :ordered_set,
    index: [:map_vnum],
    attributes: @db_attributes ++ Map.keys(@virtual_attributes)

  alias DatabaseService.Players.Character, as: DBCharacter

  @type t :: %__MODULE__{
          # DB attributes
          id: pos_integer,
          name: String.t(),
          gender: atom,
          class: atom,
          hair_color: atom,
          hair_style: atom,
          faction: atom,
          map_vnum: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          level: non_neg_integer,
          job_level: non_neg_integer,
          hero_level: non_neg_integer,
          level_xp: non_neg_integer,
          job_level_xp: non_neg_integer,
          hero_level_xp: non_neg_integer,
          gold: non_neg_integer,
          bank_gold: non_neg_integer,
          # Virtual attributes
          socket: Socket.t(),
          map_id: pos_integer,
          speed: non_neg_integer,
          direction: EntityEnums.direction_type_keys()
        }

  ## Public API

  @spec new(DBCharacter.t(), Socket.t()) :: __MODULE__.t()
  def new(%DBCharacter{} = character, %Socket{} = socket) do
    default = %{
      socket: socket,
      map_id: character.map_vnum
    }

    character
    |> Map.take(@db_attributes)
    |> Map.merge(@virtual_attributes)
    |> Map.merge(default)
    |> then(&struct!(__MODULE__, &1))
  end

  @spec get_position(__MODULE__.t()) :: Position.t()
  def get_position(%__MODULE__{} = character) do
    # TODO: Implement instances
    %Position{
      map_id: character.map_id,
      map_vnum: character.map_vnum,
      map_x: character.map_x,
      map_y: character.map_y,
      is_instance: character.map_id != character.map_vnum
    }
  end
end
