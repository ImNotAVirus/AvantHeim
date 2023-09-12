defmodule GameService.PlayerBundle do
  @moduledoc """
  TODO: Documentation for GameService.PlayerBundle
  """

  alias __MODULE__
  alias ElvenGard.ECS.{Component, Entity}
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

  ## PlayerBundle structures (for outside use)

  @enforce_keys [
    :id,
    :account,
    :endpoint,
    :player,
    :faction,
    :position,
    :level,
    :job_level,
    :hero_level,
    :currency,
    :speed,
    :direction,
    :combat,
    :size,
    :reputation,
    :sitting,
    :arena_winner,
    :family,
    :title,
    :fairy,
    :specialist,
    :invisibility,
    :cannot_attack,
    :cannot_move
  ]
  defstruct @enforce_keys

  @typep component(module) :: module | :unset
  @typep maybe_component(module) :: component(module) | nil
  @type t :: %PlayerBundle{
          id: pos_integer(),
          # Basics components
          account: component(P.AccountComponent),
          endpoint: component(P.EndpointComponent),
          player: component(P.PlayerComponent),
          faction: component(P.FactionComponent),
          position: component(E.PositionComponent),
          level: component(E.LevelComponent),
          job_level: component(P.JobLevelComponent),
          hero_level: component(P.HeroLevelComponent),
          currency: component(P.CurrencyComponent),
          speed: component(E.SpeedComponent),
          direction: component(E.DirectionComponent),
          # Hardcoded components
          combat: component(E.CombatComponent),
          size: component(P.SizeComponent),
          reputation: component(P.ReputationComponent),
          # Optional components
          sitting: maybe_component(E.SittingComponent),
          arena_winner: maybe_component(P.ArenaWinnerComponent),
          family: maybe_component(P.FamilyComponent),
          title: maybe_component(P.TitleComponent),
          fairy: maybe_component(P.FairyComponent),
          specialist: maybe_component(P.SpecialistComponent),
          invisibility: maybe_component(E.InvisibilityComponent),
          cannot_attack: maybe_component(E.CannotAttackComponent),
          cannot_move: maybe_component(E.CannotMoveComponent)
        }

  ## Public API

  @spec new(map(), map(), pid()) :: ElvenGard.ECS.Entity.spec()
  def new(attrs, account, frontend) do
    id = Map.fetch!(attrs, :id)

    Entity.entity_spec(
      id: {:player, id},
      components:
        [
          # Basics components
          {P.AccountComponent, account_specs(account)},
          {P.EndpointComponent, frontend_specs(frontend)},
          {P.PlayerComponent, player_specs(attrs)},
          {P.FactionComponent, faction_specs(attrs)},
          {E.PositionComponent, position_specs(attrs)},
          {E.LevelComponent, level_specs(attrs)},
          {P.JobLevelComponent, job_level_specs(attrs)},
          {P.HeroLevelComponent, hero_level_specs(attrs)},
          {P.CurrencyComponent, currency_specs(attrs)},
          {E.SpeedComponent, speed_specs(attrs)},
          {E.DirectionComponent, direction_specs(attrs)},
          # Hardcoded components
          {E.CombatComponent, combat_specs(attrs)},
          {P.SizeComponent, size_specs(attrs)},
          {P.ReputationComponent, reputation_specs(attrs)}
        ] ++ additional_components(attrs)
    )
  end

  @doc """
  This function can be use to create a PlayerBundle from an Entity an a list of components

  NOTE: You must verify that you have the required components in your system.
  Some components can be set set to `:unset`.
  """
  @spec load(Entity.t(), [Component.t()]) :: t()
  def load(%Entity{id: {:player, id}}, components) when is_list(components) do
    mapping = Enum.group_by(components, & &1.__struct__)

    %PlayerBundle{
      id: id,
      # Basics components
      account: Map.get(mapping, P.AccountComponent, :unset),
      endpoint: Map.get(mapping, P.EndpointComponent, :unset),
      player: Map.get(mapping, P.PlayerComponent, :unset),
      faction: Map.get(mapping, P.FactionComponent, :unset),
      position: Map.get(mapping, E.PositionComponent, :unset),
      level: Map.get(mapping, E.LevelComponent, :unset),
      job_level: Map.get(mapping, P.JobLevelComponent, :unset),
      hero_level: Map.get(mapping, P.HeroLevelComponent, :unset),
      currency: Map.get(mapping, P.CurrencyComponent, :unset),
      speed: Map.get(mapping, E.SpeedComponent, :unset),
      direction: Map.get(mapping, E.DirectionComponent, :unset),
      # Hardcoded components
      combat: Map.get(mapping, E.CombatComponent, :unset),
      size: Map.get(mapping, P.SizeComponent, :unset),
      reputation: Map.get(mapping, P.ReputationComponent, :unset),
      # Optional components
      sitting: Map.get(mapping, E.SittingComponent, :unset),
      arena_winner: Map.get(mapping, P.ArenaWinnerComponent, :unset),
      family: Map.get(mapping, P.FamilyComponent, :unset),
      title: Map.get(mapping, P.TitleComponent, :unset),
      fairy: Map.get(mapping, P.FairyComponent, :unset),
      specialist: Map.get(mapping, P.SpecialistComponent, :unset),
      invisibility: Map.get(mapping, P.InvisibilityComponent, :unset),
      cannot_attack: Map.get(mapping, E.CannotAttackComponent, :unset),
      cannot_move: Map.get(mapping, E.CannotMoveComponent, :unset)
    }
  end

  ## Getters

  def morph(%PlayerBundle{} = player) do
    case player.specialist do
      :unset -> raise ArgumentError, "you must fetch the Player.SpecialistComponent first"
      nil -> :default
      specialist -> specialist.type
    end
  end

  def morph_upgrade(%PlayerBundle{} = player) do
    case player.specialist do
      :unset -> raise ArgumentError, "you must fetch the Player.SpecialistComponent first"
      nil -> 0
      specialist -> specialist.upgrade
    end
  end

  def wings_design(%PlayerBundle{} = player) do
    case player.specialist do
      :unset -> raise ArgumentError, "you must fetch the Player.SpecialistComponent first"
      nil -> :default
      specialist -> specialist.wings_design
    end
  end

  def arena_winner?(%PlayerBundle{} = player) do
    case player.arena_winner do
      :unset -> raise ArgumentError, "you must fetch the Player.ArenaWinnerComponent first"
      arena_winner -> not is_nil(arena_winner)
    end
  end

  def size(%PlayerBundle{} = player) do
    case player.size do
      :unset -> raise ArgumentError, "you must fetch the Player.SizeComponent first"
      size -> size.value
    end
  end

  def item_morph(%PlayerBundle{} = _player) do
    # FIXME: Hardcoded value
    0
  end

  def can_attack(%PlayerBundle{} = player) do
    case player.cannot_attack do
      :unset -> raise ArgumentError, "you must fetch the Entity.CannotAttackComponent first"
      cannot_attack -> is_nil(cannot_attack)
    end
  end

  def can_move(%PlayerBundle{} = player) do
    case player.cannot_move do
      :unset -> raise ArgumentError, "you must fetch the Entity.CannotMoveComponent first"
      cannot_move -> is_nil(cannot_move)
    end
  end

  def speed(%PlayerBundle{} = player) do
    case player.speed do
      :unset -> raise ArgumentError, "you must fetch the Entity.SpeedComponent first"
      speed -> speed.value
    end
  end

  def direction(%PlayerBundle{} = player) do
    case player.direction do
      :unset -> raise ArgumentError, "you must fetch the Entity.DirectionComponent first"
      direction -> direction.value
    end
  end

  def hp(%PlayerBundle{} = player) do
    case player.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.hp
    end
  end

  def hp_max(%PlayerBundle{} = player) do
    case player.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.hp_max
    end
  end

  def mp(%PlayerBundle{} = player) do
    case player.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.mp
    end
  end

  def mp_max(%PlayerBundle{} = player) do
    case player.combat do
      :unset -> raise ArgumentError, "you must fetch the Entity.CombatComponent first"
      combat -> combat.mp_max
    end
  end

  def level(%PlayerBundle{} = player) do
    case player.level do
      :unset -> raise ArgumentError, "you must fetch the Entity.LevelComponent first"
      level -> level.value
    end
  end

  def job_level(%PlayerBundle{} = player) do
    case player.job_level do
      :unset -> raise ArgumentError, "you must fetch the Player.JobLevelComponent first"
      job_level -> job_level.value
    end
  end

  def hero_level(%PlayerBundle{} = player) do
    case player.hero_level do
      :unset -> raise ArgumentError, "you must fetch the Player.HeroLevelComponent first"
      hero_level -> hero_level.value
    end
  end

  ## Components specs

  defp account_specs(%{id: id, username: username, authority: authority}) do
    [id: id, username: username, authority: authority]
  end

  defp frontend_specs(pid) when is_pid(pid) do
    [pid: pid]
  end

  defp player_specs(attrs) do
    %{
      name: name,
      gender: gender,
      class: class,
      hair_color: hair_color,
      hair_style: hair_style
    } = attrs

    [name: name, gender: gender, class: class, hair_color: hair_color, hair_style: hair_style]
  end

  defp faction_specs(%{faction: value}) do
    [value: value]
  end

  defp position_specs(%{map_id: map_id, map_x: map_x, map_y: map_y} = attrs) do
    map_ref = Map.get(attrs, :map_ref, map_id)
    [map_id: map_id, map_ref: map_ref, map_x: map_x, map_y: map_y]
  end

  defp level_specs(%{level: value, level_xp: xp}) do
    # FIXME: Harcoded value
    [value: value, xp: xp, xp_max: 10_000]
  end

  defp job_level_specs(%{job_level: value, job_level_xp: xp}) do
    # FIXME: Harcoded value
    [value: value, xp: xp, xp_max: 10_000]
  end

  defp hero_level_specs(%{hero_level: value, hero_level_xp: xp}) do
    # FIXME: Harcoded value
    [value: value, xp: xp, xp_max: 0]
  end

  defp currency_specs(%{gold: gold, bank_gold: bank_gold}) do
    [gold: gold, bank_gold: bank_gold]
  end

  defp speed_specs(attrs) do
    # FIXME: Hardcoded value, now sure if it's the best place
    [value: Map.get(attrs, :speed, 20)]
  end

  defp direction_specs(attrs) do
    [value: Map.get(attrs, :direction, :south)]
  end

  # Hardcoded components specs

  defp combat_specs(_attrs) do
    [
      hp: 8_000,
      hp_max: 10_000,
      mp: 4_500,
      mp_max: 5_000
    ]
  end

  defp size_specs(_attrs) do
    [value: 10]
  end

  defp reputation_specs(attrs) do
    case attrs.id do
      1 ->
        [
          dignity: 100,
          dignity_icon: :basic,
          reputation: 10_000_000,
          reputation_icon: :legendary_hero,
          compliment: 500
        ]

      _ ->
        [
          dignity: 100,
          dignity_icon: :basic,
          reputation: 1_000,
          reputation_icon: :green_beginner,
          compliment: 100
        ]
    end
  end

  # Additional Components

  defp additional_components(attrs) do
    case attrs.id do
      1 ->
        [
          P.ArenaWinnerComponent,
          {P.FamilyComponent, family_specs(attrs)},
          {P.TitleComponent, title_specs(attrs)},
          {P.FairyComponent, fairy_specs(attrs)}
        ]

      _ ->
        [{P.SpecialistComponent, specialist_specs(attrs)}]
    end
  end

  defp specialist_specs(_attrs) do
    [type: :demon_hunter, upgrade: 15, wings_design: :tree]
  end

  defp family_specs(_attrs) do
    [id: 1337, name: "Alchemists", rank: :head, level: 20, icons: [true, true, true]]
  end

  defp title_specs(_attrs) do
    [id: 10]
  end

  defp fairy_specs(_attrs) do
    # FIXME: Use enums for type and move type
    [type: 13, move_type: 2, element: :darkness]
  end
end
