defmodule GameService.SystemCase do
  use ExUnit.CaseTemplate

  alias ElvenGard.ECS.{Entity, Command}
  alias GameService.EntityComponents, as: E
  alias GameService.PlayerComponents, as: P
  alias GameService.MonsterComponents, as: M

  using _ do
    quote do
      import unquote(__MODULE__),
        only: [spawn_player: 0, spawn_player: 1, spawn_monster: 0, spawn_monster: 1]

      alias ElvenGard.ECS.{Command, Entity, Query}

      alias GameService.Events, as: Evt
      alias GameService.EntityComponents, as: E
      alias GameService.PlayerComponents, as: P
      alias GameService.MonsterComponents, as: M
    end
  end

  ## Public API (imported via `use`)

  def spawn_player(attrs \\ []) do
    map_components =
      attrs
      |> Keyword.get(:components, [])
      |> Enum.group_by(& &1.__struct__, & &1)

    map_default = Enum.group_by(default_player_components(), & &1.__struct__, & &1)

    components =
      map_default
      |> Map.merge(map_components)
      |> Map.values()
      |> List.flatten()

    {:ok, {entity, _components}} =
      attrs
      |> Keyword.put(:components, components)
      |> Entity.entity_spec()
      |> Map.update!(:id, &{:player, &1})
      |> Command.spawn_entity()

    entity
  end

  def spawn_monster(attrs \\ []) do
    map_components =
      attrs
      |> Keyword.get(:components, [])
      |> Enum.group_by(& &1.__struct__, & &1)

    map_default = Enum.group_by(default_monster_components(), & &1.__struct__, & &1)

    components =
      map_default
      |> Map.merge(map_components)
      |> Map.values()
      |> List.flatten()

    {:ok, {entity, _components}} =
      attrs
      |> Keyword.put(:components, components)
      |> Entity.entity_spec()
      |> Map.update!(:id, &{:monster, &1})
      |> Command.spawn_entity()

    entity
  end

  ## Private function

  defp default_monster_components() do
    [
      %M.MonsterComponent{name: "Danderito", vnum: 1, spawn_effect: :falling},
      %E.PositionComponent{map_id: 123, map_ref: make_ref(), map_x: 50, map_y: 12},
      %E.LevelComponent{value: 40, xp: 403_000, xp_max: 5_000_000},
      %E.SpeedComponent{value: 14},
      %E.DirectionComponent{value: :north},
      %E.CombatComponent{hp: 40_000, hp_max: 44_444, mp: 30_000, mp_max: 33_333}
    ]
  end

  defp default_player_components() do
    [
      %P.AccountComponent{id: 123, username: "username", authority: :player},
      %P.EndpointComponent{pid: elem(Task.start(fn -> :ok end), 1)},
      %P.PlayerComponent{
        name: "PlayerName",
        gender: :male,
        class: :archer,
        hair_color: :dark_purple,
        hair_style: :hair_style_a
      },
      %P.FactionComponent{value: :demon},
      %E.PositionComponent{map_id: 123, map_ref: make_ref(), map_x: 12, map_y: 34},
      %E.LevelComponent{value: 99, xp: 999_999, xp_max: 9_000_000},
      %P.JobLevelComponent{value: 89, xp: 888_888, xp_max: 8_000_000},
      %P.HeroLevelComponent{value: 79, xp: 777_777, xp_max: 7_000_000},
      %P.GoldComponent{value: 666},
      %P.BankComponent{gold: 123_456_789, rank: 10, tax: 20},
      %E.SpeedComponent{value: 40},
      %E.DirectionComponent{value: :south},
      %E.CombatComponent{hp: 40_000, hp_max: 44_444, mp: 30_000, mp_max: 33_333},
      %P.SizeComponent{value: 20},
      %P.ReputationComponent{
        dignity: 100,
        dignity_icon: :basic,
        reputation: 111_111,
        reputation_icon: :blue_nos,
        compliment: 500
      }
    ]
  end
end
