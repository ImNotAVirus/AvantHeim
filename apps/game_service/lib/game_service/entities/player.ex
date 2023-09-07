defmodule GameService.PlayerEntity do
  @moduledoc """
  TODO: Documentation for GameService.PlayerEntity
  """

  alias __MODULE__
  alias ElvenGard.ECS.{Component, Entity}
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

  ## PlayerEntity structures (for outside use)

  @enforce_keys [:id, :components]
  defstruct [:id, :components]

  @type t :: %PlayerEntity{id: pos_integer(), components: [Component.t()]}

  ## Public API

  @spec new(map(), map(), pid()) :: ElvenGard.ECS.Entity.spec()
  def new(attrs, account, frontend) do
    id = attrs[:id] || raise ArgumentError, ":id attribute is required"

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
          {E.SittingComponent, sitting_specs(attrs)},
          # Hardcoded components
          {E.CombatComponent, combat_specs(attrs)},
          {P.SizeComponent, size_specs(attrs)},
          {P.ReputationComponent, reputation_specs(attrs)}
        ] ++ additional_components(attrs)
    )
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

  defp faction_specs(%{faction: faction}) do
    [faction: faction]
  end

  defp position_specs(%{map_id: map_id, map_x: map_x, map_y: map_y} = attrs) do
    map_ref = Map.get(attrs, :map_ref, map_id)
    [map_id: map_id, map_ref: map_ref, map_x: map_x, map_y: map_y]
  end

  defp level_specs(%{level: level, level_xp: xp}) do
    # FIXME: Harcoded value
    [level: level, xp: xp, xp_max: 10_000]
  end

  defp job_level_specs(%{job_level: level, job_level_xp: xp}) do
    # FIXME: Harcoded value
    [level: level, xp: xp, xp_max: 10_000]
  end

  defp hero_level_specs(%{hero_level: level, hero_level_xp: xp}) do
    # FIXME: Harcoded value
    [level: level, xp: xp, xp_max: 0]
  end

  defp currency_specs(%{gold: gold, bank_gold: bank_gold}) do
    [gold: gold, bank_gold: bank_gold]
  end

  defp speed_specs(%{speed: value}) do
    [value: value]
  end

  defp direction_specs(%{direction: value}) do
    [value: value]
  end

  defp sitting_specs(%{is_sitting: value}) do
    [value: value]
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
