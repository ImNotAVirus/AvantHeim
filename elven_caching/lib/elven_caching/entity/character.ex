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
  alias ElvenEnums.{EntityEnums, PlayerEnums}
  alias ElvenCaching.Entity.EntityPosition

  # Protocols
  alias ElvenCaching.Entity
  alias ElvenCaching.MapEntity
  alias ElvenCaching.LevelableEntity

  @type t :: %Character{
          # Required attributes
          id: Entity.entity_id(),
          account_id: pos_integer,
          name: String.t(),
          gender: PlayerEnums.gender_keys(),
          class: PlayerEnums.character_class_keys(),
          hair_color: PlayerEnums.hair_color_keys(),
          hair_style: PlayerEnums.hair_style_keys(),
          faction: PlayerEnums.faction_keys(),
          map_vnum: EntityPosition.map_vnum(),
          map_x: EntityPosition.map_axis(),
          map_y: EntityPosition.map_axis(),
          level: LevelableEntity.level(),
          job_level: LevelableEntity.level(),
          hero_level: LevelableEntity.level(),
          level_xp: non_neg_integer,
          job_level_xp: non_neg_integer,
          hero_level_xp: non_neg_integer,
          gold: non_neg_integer,
          bank_gold: non_neg_integer,
          socket: Socket.t(),
          # Virtual attributes
          map_id: EntityPosition.map_id(),
          speed: MapEntity.speed(),
          direction: MapEntity.direction()
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
    def type(_), do: :character
    def id(%Character{id: id}), do: id
  end

  defimpl ElvenCaching.MapEntity do
    import EntityEnums, only: [direction_type: 1]

    def size(character), do: FakeData.size(character_id: character.id)
    def size(_character, _size), do: raise("unimplemented setter for size")

    def direction(%Character{direction: direction}), do: direction

    def direction(character, direction) when direction in direction_type(:__values__) do
      %Character{character | direction: direction}
    end

    def speed(%Character{speed: speed}), do: speed

    def speed(character, speed) when is_integer(speed) do
      %Character{character | speed: speed}
    end

    def position(character) do
      EntityPosition.new(
        character.map_id,
        character.map_vnum,
        character.map_x,
        character.map_y
      )
    end

    def position(character, %EntityPosition{} = pos) do
      %Character{
        character
        | map_id: pos.map_id,
          map_vnum: pos.map_vnum,
          map_x: pos.map_x,
          map_y: pos.map_y
      }
    end
  end

  defimpl ElvenCaching.LevelableEntity do
    def level(%Character{level: level}), do: level

    def level(character, level) when is_integer(level) do
      %Character{character | level: level}
    end

    def hero_level(%Character{hero_level: hero_level}), do: hero_level

    def hero_level(character, hero_level) when is_integer(hero_level) do
      %Character{character | hero_level: hero_level}
    end
  end

  defimpl ElvenCaching.BattleEntity do
    def hp(character), do: FakeData.hp(character_id: character.id)
    def hp(_character, _hp), do: raise("unimplemented setter for hp")

    def hp_max(character), do: FakeData.hp_max(character_id: character.id)
    def hp_max(_character, _hp_max), do: raise("unimplemented setter for hp_max")

    def mp(character), do: FakeData.mp(character_id: character.id)
    def mp(_character, _mp), do: raise("unimplemented setter for mp")

    def mp_max(character), do: FakeData.mp_max(character_id: character.id)
    def mp_max(_character, _mp_max), do: raise("unimplemented setter for mp_max")

    def can_attack(character), do: FakeData.can_attack(character_id: character.id)
    def can_attack(_character, _can_attack), do: raise("unimplemented setter for can_attack")

    def can_move(character), do: FakeData.can_move(character_id: character.id)
    def can_move(_character, _can_move), do: raise("unimplemented setter for can_move")
  end
end
