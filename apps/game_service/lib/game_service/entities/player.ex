defmodule GameService.PlayerEntity do
  @moduledoc """
  TODO: Documentation for GameService.PlayerEntity
  """

  alias __MODULE__
  alias ElvenGard.ECS.{Component, Entity}
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

  ## PlayerEntity structures (for outside use)

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

  @typep maybe(component) :: component | nil
  @type t :: %PlayerEntity{
          id: pos_integer(),
          # Basics components
          account: P.AccountComponent,
          endpoint: P.EndpointComponent,
          player: P.PlayerComponent,
          faction: P.FactionComponent,
          position: E.PositionComponent,
          level: E.LevelComponent,
          job_level: P.JobLevelComponent,
          hero_level: P.HeroLevelComponent,
          currency: P.CurrencyComponent,
          speed: E.SpeedComponent,
          direction: E.DirectionComponent,
          # Hardcoded components
          combat: E.CombatComponent,
          size: P.SizeComponent,
          reputation: P.ReputationComponent,
          # Optional components
          sitting: maybe(E.SittingComponent),
          arena_winner: maybe(P.ArenaWinnerComponent),
          family: maybe(P.FamilyComponent),
          title: maybe(P.TitleComponent),
          fairy: maybe(P.FairyComponent),
          specialist: maybe(P.SpecialistComponent),
          invisibility: maybe(E.InvisibilityComponent),
          cannot_attack: maybe(E.CannotAttackComponent),
          cannot_move: maybe(E.CannotMoveComponent)
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
  This function can be use to create a PlayerEntity from an Entity an a list of components

  NOTE: This can produce an invalid PlayerEntity and you must verify that you have the required
  components in your system.
  """
  @spec part_load(Entity.t(), [Component.t()]) :: t()
  def part_load(%Entity{id: {:player, id}}, components) when is_list(components) do
    mapping = Enum.group_by(components, & &1.__struct__)

    %PlayerEntity{
      id: id,
      # Basics components
      account: Map.get(mapping, P.AccountComponent),
      endpoint: Map.get(mapping, P.EndpointComponent),
      player: Map.get(mapping, P.PlayerComponent),
      faction: Map.get(mapping, P.FactionComponent),
      position: Map.get(mapping, E.PositionComponent),
      level: Map.get(mapping, E.LevelComponent),
      job_level: Map.get(mapping, P.JobLevelComponent),
      hero_level: Map.get(mapping, P.HeroLevelComponent),
      currency: Map.get(mapping, P.CurrencyComponent),
      speed: Map.get(mapping, E.SpeedComponent),
      direction: Map.get(mapping, E.DirectionComponent),
      # Hardcoded components
      combat: Map.get(mapping, E.CombatComponent),
      size: Map.get(mapping, P.SizeComponent),
      reputation: Map.get(mapping, P.ReputationComponent),
      # Optional components
      sitting: Map.get(mapping, E.SittingComponent),
      arena_winner: Map.get(mapping, P.ArenaWinnerComponent),
      family: Map.get(mapping, P.FamilyComponent),
      title: Map.get(mapping, P.TitleComponent),
      fairy: Map.get(mapping, P.FairyComponent),
      specialist: Map.get(mapping, P.SpecialistComponent),
      invisibility: Map.get(mapping, P.InvisibilityComponent),
      cannot_attack: Map.get(mapping, E.CannotAttackComponent),
      cannot_move: Map.get(mapping, E.CannotMoveComponent)
    }
  end

  ## Getters

  def morph(%PlayerEntity{} = player) do
    case player.specialist do
      nil -> :default
      specialist -> specialist.type
    end
  end

  def morph_upgrade(%PlayerEntity{} = player) do
    case player.specialist do
      nil -> 0
      specialist -> specialist.upgrade
    end
  end

  def wings_design(%PlayerEntity{} = player) do
    case player.specialist do
      nil -> :default
      specialist -> specialist.wings_design
    end
  end

  def arena_winner?(%PlayerEntity{} = player) do
    not is_nil(player.arena_winner)
  end

  def size(%PlayerEntity{} = player) do
    player.size.value
  end

  def item_morph(%PlayerEntity{} = _player) do
    # FIXME: Hardcoded value
    0
  end

  def can_attack(%PlayerEntity{} = player) do
    is_nil(player.cannot_attack)
  end

  def can_move(%PlayerEntity{} = player) do
    is_nil(player.cannot_move)
  end

  def speed(%PlayerEntity{} = player) do
    player.speed.value
  end

  def direction(%PlayerEntity{} = player) do
    player.direction.value
  end

  def hp(%PlayerEntity{} = player) do
    player.combat.hp
  end

  def hp_max(%PlayerEntity{} = player) do
    player.combat.hp_max
  end

  def mp(%PlayerEntity{} = player) do
    player.combat.mp
  end

  def mp_max(%PlayerEntity{} = player) do
    player.combat.mp_max
  end

  def level(%PlayerEntity{} = player) do
    player.level.value
  end

  def job_level(%PlayerEntity{} = player) do
    player.job_level.value
  end

  def hero_level(%PlayerEntity{} = player) do
    player.hero_level.value
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
