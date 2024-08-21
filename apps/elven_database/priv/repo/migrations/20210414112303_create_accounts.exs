defmodule ElvenDatabase.Repo.Migrations.CreateAccounts do
  use Ecto.Migration

  require ElvenDatabase.EctoEnumHelpers, as: EctoEnumHelpers
  require ElvenData.Enums.PlayerEnums, as: PlayerEnums

  def change() do
    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :authority),
      EctoEnumHelpers.drop_query(:authority)
    )

    execute(
      EctoEnumHelpers.create_query(PlayerEnums, :language),
      EctoEnumHelpers.drop_query(:language)
    )

    create table(:accounts) do
      add :username, :string, null: false
      add :hashed_password, :string, null: false, size: 128
      add :authority, :authority_enum, null: false, default: "player"
      add :language, :language_enum, null: false, default: "en"

      timestamps()
    end

    create unique_index(:accounts, [:username])
  end
end
