defmodule CachingService.Player.Character do
  @moduledoc """
  TODO: Documentation
  """

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
    :hero_level_xp
  ]

  @virtual_attributes [
    :speed
  ]

  use Memento.Table,
    type: :ordered_set,
    index: [:map_vnum],
    attributes: @db_attributes ++ @virtual_attributes

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
          # Virtual attributes
          speed: non_neg_integer
        }

  ## Public API

  @spec new(DBCharacter.t()) :: __MODULE__.t()
  def new(%DBCharacter{} = character) do
    default_values = %{
      speed: 20
    }

    character
    |> Map.take(@db_attributes)
    |> then(&struct!(__MODULE__, &1))
    |> Map.merge(default_values)
  end

  ## TODO: get_position
end
