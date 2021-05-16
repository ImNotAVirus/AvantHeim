defmodule DatabaseService.Repo.Migrations.CreateCharacters do
  use Ecto.Migration

  require DatabaseService.EctoEnumHelpers
  require DatabaseService.PlayerEnums

  alias DatabaseService.{EctoEnumHelpers, PlayerEnums}

  def change do
    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :character_class),
      EctoEnumHelpers.drop_query(:character_class)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :faction),
      EctoEnumHelpers.drop_query(:faction)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :gender),
      EctoEnumHelpers.drop_query(:gender)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :hair_color),
      EctoEnumHelpers.drop_query(:hair_color)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :hair_style),
      EctoEnumHelpers.drop_query(:hair_style)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :miniland_state),
      EctoEnumHelpers.drop_query(:miniland_state)
    )

    create table(:characters) do
      add :account_id, references(:accounts), null: false
      add :name, :string, size: 32, null: false
      add :slot, :int2, null: false
      add :disabled, :boolean, default: false, null: false

      add :class, :character_class_enum, default: "adventurer", null: false
      add :faction, :faction_enum, default: "neutral", null: false
      add :gender, :gender_enum, null: false
      add :hair_color, :hair_color_enum, null: false
      add :hair_style, :hair_style_enum, null: false

      add :map_id, :int2, null: false
      add :map_x, :int2, null: false
      add :map_y, :int2, null: false

      add :additional_hp, :int4, default: 0, null: false
      add :additional_mp, :int4, default: 0, null: false
      add :gold, :int8, default: 0, null: false
      add :biography, :string, default: "Hi!", null: false

      add :level, :int2, default: 1, null: false
      add :job_level, :int2, default: 1, null: false
      add :hero_level, :int2, default: 0, null: false
      add :level_xp, :int4, default: 0, null: false
      add :job_level_xp, :int4, default: 0, null: false
      add :hero_level_xp, :int4, default: 0, null: false

      add :sp_points, :int4, default: 10_000, null: false
      add :sp_additional_points, :int4, default: 50_000, null: false
      add :rage_points, :int4, default: 0, null: false
      add :max_mate_count, :int2, default: 10, null: false

      add :reputation, :int4, default: 0, null: false
      add :dignity, :int2, default: 100, null: false
      add :compliment, :int2, default: 0, null: false

      add :act4_dead, :int4, default: 0, null: false
      add :act4_kill, :int4, default: 0, null: false
      add :act4_points, :int4, default: 0, null: false
      add :arena_winner, :boolean, default: false, null: false
      add :talent_win, :int4, default: 0, null: false
      add :talent_lose, :int4, default: 0, null: false
      add :talent_surrender, :int4, default: 0, null: false
      add :master_points, :int4, default: 0, null: false
      add :master_ticket, :int4, default: 0, null: false

      add :miniland_intro, :string, default: "Welcome!", null: false
      add :miniland_state, :miniland_state_enum, default: "open", null: false
      add :miniland_makepoints, :int2, default: 2000, null: false

      # add :game_options, :int8, default: 0, null: false

      timestamps()
    end

    create unique_index(:characters, [:name])
  end
end
