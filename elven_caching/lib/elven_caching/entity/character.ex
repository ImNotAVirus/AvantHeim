defmodule ElvenCaching.Entity.Character do
  @moduledoc """
  TODO: Documentation
  """

  @required_attributes [
    :id,
    :account_id,
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
    :bank_gold,
    :socket
  ]

  @virtual_attributes %{
    map_id: nil,
    speed: 20,
    direction: :south
  }

  use Memento.Table,
    type: :ordered_set,
    index: [:account_id, :name, :map_id],
    attributes: @required_attributes ++ Map.keys(@virtual_attributes)

  alias __MODULE__
  alias ElvenCore.Socket
  alias ElvenCaching.Entity.EntityPosition
  alias ElvenEnums.{EntityEnums, PlayerEnums}

  @type t :: %Character{
          # Required attributes
          id: pos_integer,
          account_id: pos_integer,
          name: String.t(),
          gender: PlayerEnums.gender_keys(),
          class: PlayerEnums.character_class_keys(),
          hair_color: PlayerEnums.hair_color_keys(),
          hair_style: PlayerEnums.hair_style_keys(),
          faction: PlayerEnums.faction_keys(),
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
          socket: Socket.t(),
          # Virtual attributes
          map_id: EntityPosition.map_id(),
          speed: non_neg_integer,
          direction: EntityEnums.direction_type_keys()
        }

  ## Public API

  @spec new(map) :: t()
  def new(attrs) do
    default = %{map_id: attrs.map_vnum}

    attrs
    |> extract_attributes!(@required_attributes)
    |> Map.merge(@virtual_attributes)
    |> Map.merge(default)
    |> then(&struct!(Character, &1))
  end

  defp extract_attributes!(attrs, required_attributes) do
    case required_attributes -- Map.keys(attrs) do
      [] -> Map.take(attrs, required_attributes)
      missing -> raise ArgumentError, "missing attributes: #{inspect(missing)}"
    end
  end

  ## Implement protocols

  defimpl ElvenCaching.Entity do
    def type(%Character{}), do: :character
    def id(%Character{id: id}), do: id
  end

  defimpl ElvenCaching.MovableEntity do
    def position(%Character{} = character) do
      EntityPosition.new(
        character.map_id,
        character.map_vnum,
        character.map_x,
        character.map_y
      )
    end

    def position(%Character{} = character, %EntityPosition{} = pos) do
      %Character{
        character
        | map_id: pos.map_id,
          map_vnum: pos.map_vnum,
          map_x: pos.map_x,
          map_y: pos.map_y
      }
    end
  end
end
