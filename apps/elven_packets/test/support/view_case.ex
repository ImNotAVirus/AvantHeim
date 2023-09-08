defmodule ElvenPackets.ViewCase do
  use ExUnit.CaseTemplate

  alias GameService.PlayerEntity
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P

  ## Public API

  using do
    quote do
      import unquote(__MODULE__), only: [new_player: 0]
    end
  end

  ## Helpers

  def new_player(attrs \\ %{}) do
    %PlayerEntity{
      id: Enum.random(1..100_000),
      # Basics components
      account: account_component(),
      endpoint: endpoint_component(),
      player: player_component(),
      faction: faction_component(),
      position: position_component(),
      level: level_component(),
      job_jevel: job_jevel_component(),
      hero_level: hero_level_component(),
      currency: currency_component(),
      speed: speed_component(),
      direction: direction_component(),
      # Hardcoded components
      combat: combat_component(),
      size: size_component(),
      reputation: reputation_component(),
      # Optional components
      sitting: nil,
      arena_winner: nil,
      family: nil,
      title: nil,
      fairy: nil,
      specialist: nil,
      invisibility: nil,
      cannot_attack: nil,
      cannot_move: nil
    }
    |> Map.merge(attrs)
  end

  defp account_component() do
    %P.AccountComponent{id: 123, username: "username", authority: :player}
  end

  defp endpoint_component() do
    %P.EndpointComponent{pid: self()}
  end

  defp player_component() do
    %P.PlayerComponent{
      name: "PlayerName",
      gender: :male,
      class: :archer,
      hair_color: :dark_purple,
      hair_style: :hair_style_a
    }
  end

  defp faction_component() do
    %P.FactionComponent{value: :demon}
  end

  defp position_component() do
    %E.PositionComponent{map_id: 123, map_ref: make_ref(), map_x: 12, map_y: 34}
  end

  defp level_component() do
    %E.LevelComponent{value: 99, xp: 999_999, xp_max: 9_000_000}
  end

  defp job_jevel_component() do
    %P.JobLevelComponent{value: 89, xp: 888_888, xp_max: 8_000_000}
  end

  defp hero_level_component() do
    %P.HeroLevelComponent{value: 79, xp: 777_777, xp_max: 7_000_000}
  end

  defp currency_component() do
    %P.CurrencyComponent{gold: 666, bank_gold: 555}
  end

  defp speed_component() do
    %E.SpeedComponent{value: 40}
  end

  defp direction_component() do
    %E.DirectionComponent{value: :south}
  end

  defp combat_component() do
    %E.CombatComponent{hp: 40_000, hp_max: 44_444, mp: 30_000, mp_max: 33_333}
  end

  defp size_component() do
    %P.SizeComponent{value: 20}
  end

  defp reputation_component() do
    %P.ReputationComponent{
      dignity: 100,
      dignity_icon: :basic,
      reputation: 111_111,
      reputation_icon: :blue_nos,
      compliment: 500
    }
  end
end
