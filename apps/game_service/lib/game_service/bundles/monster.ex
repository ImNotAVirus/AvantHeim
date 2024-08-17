defmodule GameService.MonsterBundle do
  @moduledoc """
  TODO: Documentation for GameService.MonsterBundle
  """

  alias __MODULE__
  alias ElvenGard.ECS.{Component, Entity}
  alias GameService.EntityComponents, as: E
  alias GameService.MonsterComponents, as: M

  ## MonsterBundle structures (for outside use)

  @enforce_keys [
    :id,
    :monster,
    :position,
    :level,
    :speed,
    :direction,
    :combat,
    :sitting,
    :invisibility,
    :cannot_attack,
    :cannot_move,
    :ai_movement
  ]
  defstruct @enforce_keys

  @typep component(module) :: module | :unset
  @typep maybe_component(module) :: component(module) | nil

  @type t :: %MonsterBundle{
          id: pos_integer(),
          # Basics components
          monster: component(M.MonsterComponent),
          position: component(E.PositionComponent),
          level: component(E.LevelComponent),
          speed: component(E.SpeedComponent),
          direction: component(E.DirectionComponent),
          # Hardcoded components
          combat: component(E.CombatComponent),
          # Optional components
          sitting: maybe_component(E.SittingComponent),
          invisibility: maybe_component(E.InvisibilityComponent),
          cannot_attack: maybe_component(E.CannotAttackComponent),
          cannot_move: maybe_component(E.CannotMoveComponent),
          ai_movement: maybe_component(M.AIMovementComponent)
        }

  ## Public API

  @spec specs(map()) :: ElvenGard.ECS.Entity.spec()
  def specs(attrs) do
    id = Map.fetch!(attrs, :id)

    Entity.entity_spec(
      id: {:monster, id},
      partition: Keyword.fetch!(position_specs(attrs), :map_ref),
      components: [
        # Basics components
        {M.MonsterComponent, monster_specs(attrs)},
        {E.PositionComponent, position_specs(attrs)},
        {E.LevelComponent, level_specs(attrs)},
        {E.SpeedComponent, speed_specs(attrs)},
        {E.DirectionComponent, direction_specs(attrs)},
        # Hardcoded components
        {E.CombatComponent, combat_specs(attrs)},
        {M.AIMovementComponent, ai_movement_specs(attrs)}
      ]
    )
  end

  @doc """
  This function can be use to create a MonsterBundle from an Entity an a list of components
  """
  @spec load(Entity.t(), [Component.t()]) :: t()
  def load(%Entity{id: {:monster, id}}, components) when is_list(components) do
    # mapping = Enum.group_by(components, & &1.__struct__)
    mapping = Map.new(components, &{&1.__struct__, &1})

    %MonsterBundle{
      id: id,
      # Basics components
      monster: Map.fetch!(mapping, M.MonsterComponent),
      position: Map.fetch!(mapping, E.PositionComponent),
      level: Map.fetch!(mapping, E.LevelComponent),
      speed: Map.fetch!(mapping, E.SpeedComponent),
      direction: Map.fetch!(mapping, E.DirectionComponent),
      # Hardcoded components
      combat: Map.fetch!(mapping, E.CombatComponent),
      # Optional components
      sitting: Map.get(mapping, E.SittingComponent),
      invisibility: Map.get(mapping, E.InvisibilityComponent),
      cannot_attack: Map.get(mapping, E.CannotAttackComponent),
      cannot_move: Map.get(mapping, E.CannotMoveComponent),
      ai_movement: Map.get(mapping, M.AIMovementComponent)
    }
  end

  @doc """
  This function can be use to create a MonsterBundle from an Entity an a list of components

  Unlike `load/2`, you don't have to provide all components.  
  Components not found will have the value `:unset`

  NOTE: You must verify that you have the required components in your system.
  """
  @spec preload(Entity.t(), [Component.t()]) :: t()
  def preload(%Entity{id: {:monster, id}}, components) when is_list(components) do
    # mapping = Enum.group_by(components, & &1.__struct__)
    mapping = Map.new(components, &{&1.__struct__, &1})

    %MonsterBundle{
      id: id,
      # Basics components
      monster: Map.get(mapping, M.MonsterComponent, :unset),
      position: Map.get(mapping, E.PositionComponent, :unset),
      level: Map.get(mapping, E.LevelComponent, :unset),
      speed: Map.get(mapping, E.SpeedComponent, :unset),
      direction: Map.get(mapping, E.DirectionComponent, :unset),
      # Hardcoded components
      combat: Map.get(mapping, E.CombatComponent, :unset),
      # Optional components
      sitting: Map.get(mapping, E.SittingComponent, :unset),
      invisibility: Map.get(mapping, E.InvisibilityComponent, :unset),
      cannot_attack: Map.get(mapping, E.CannotAttackComponent, :unset),
      cannot_move: Map.get(mapping, E.CannotMoveComponent, :unset),
      ai_movement: Map.get(mapping, M.AIMovementComponent, :unset)
    }
  end

  ## Getters

  def hp(%MonsterBundle{} = monster) do
    case monster.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.hp
    end
  end

  def hp_max(%MonsterBundle{} = monster) do
    case monster.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.hp_max
    end
  end

  def mp(%MonsterBundle{} = monster) do
    case monster.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.mp
    end
  end

  def mp_max(%MonsterBundle{} = monster) do
    case monster.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.mp_max
    end
  end

  def vnum(%MonsterBundle{} = monster) do
    case monster.monster do
      :unset -> raise ArgumentError, "you must fetch the Monster.MonsterComponent first"
      monster -> monster.vnum
    end
  end

  def map_ref(%MonsterBundle{} = monster) do
    case monster.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_ref
    end
  end

  def map_x(%MonsterBundle{} = monster) do
    case monster.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_x
    end
  end

  def map_y(%MonsterBundle{} = monster) do
    case monster.position do
      :unset -> raise ArgumentError, "you must fetch the Entity.PositionComponent first"
      position -> position.map_y
    end
  end

  def sitting?(%MonsterBundle{} = monster) do
    case monster.sitting do
      :unset -> raise ArgumentError, "you must fetch the Entity.SittingComponent first"
      sitting -> not is_nil(sitting)
    end
  end

  def invisible?(%MonsterBundle{} = monster) do
    case monster.invisibility do
      :unset -> raise ArgumentError, "you must fetch the Entity.InvisibilityComponent first"
      invisibility -> not is_nil(invisibility)
    end
  end

  def name(%MonsterBundle{} = monster) do
    case monster.monster do
      :unset -> raise ArgumentError, "you must fetch the Monster.MonsterComponent first"
      monster -> monster.name
    end
  end

  def spawn_effect(%MonsterBundle{} = monster) do
    case monster.monster do
      :unset -> raise ArgumentError, "you must fetch the Monster.MonsterComponent first"
      monster -> monster.spawn_effect
    end
  end

  def direction(%MonsterBundle{} = monster) do
    case monster.direction do
      :unset -> raise ArgumentError, "you must fetch the Entity.DirectionComponent first"
      direction -> direction.value
    end
  end

  ## Components specs

  defp monster_specs(%{vnum: vnum}) do
    [vnum: vnum, spawn_effect: :falling]
  end

  defp position_specs(%{map_id: map_id, map_x: map_x, map_y: map_y} = attrs) do
    map_ref = Map.get(attrs, :map_ref, map_id)
    [map_id: map_id, map_ref: map_ref, map_x: map_x, map_y: map_y]
  end

  defp level_specs(_attrs) do
    # FIXME: Harcoded value
    [value: 3]
  end

  defp speed_specs(attrs) do
    # FIXME: Hardcoded value, now sure if it's the best place
    [value: Map.get(attrs, :speed, 10)]
  end

  defp direction_specs(attrs) do
    [value: Map.get(attrs, :direction, :south)]
  end

  # Hardcoded components specs

  defp combat_specs(_attrs) do
    [
      hp: 400,
      hp_max: 500,
      mp: 300,
      mp_max: 400
    ]
  end

  defp ai_movement_specs(%{map_x: map_x, map_y: map_y}) do
    [orig_x: map_x, orig_y: map_y, next_move: ElvenGard.ECS.now()]
  end
end
