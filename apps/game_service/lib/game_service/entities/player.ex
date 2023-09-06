defmodule GameService.PlayerEntity do
  @moduledoc """
  TODO: Documentation for GameService.PlayerEntity
  """

  alias ElvenGard.ECS.Entity
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

  @spec new(map(), map(), pid()) :: ElvenGard.ECS.Entity.spec()
  def new(attrs, account, frontend) do
    id = attrs[:id] || raise ArgumentError, ":id attribute is required"

    Entity.entity_spec(
      id: {:player, id},
      components: [
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
        {E.SittingComponent, sitting_specs(attrs)}
      ]
    )
  end

  ## Components specs

  defp account_specs(%{id: id, username: username}) do
    [id: id, username: username]
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
    [level: level, xp: xp]
  end

  defp job_level_specs(%{job_level: level, job_level_xp: xp}) do
    [level: level, xp: xp]
  end

  defp hero_level_specs(%{hero_level: level, hero_level_xp: xp}) do
    [level: level, xp: xp]
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
end
