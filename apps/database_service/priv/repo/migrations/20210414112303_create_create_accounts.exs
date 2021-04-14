defmodule DatabaseService.Repo.Migrations.CreateCreateAccounts do
  use Ecto.Migration

  def change do
    execute(
      "CREATE TYPE language_enum AS ENUM ('en', 'fr')",
      "DROP TYPE language_enum"
    )

    create table(:accounts) do
      add :username, :string, null: false
      add :password, :string, null: false, size: 128
      add :authority, :integer, null: false, default: 0
      add :language, :language_enum, null: false, default: "en"

      timestamps()
    end

    create unique_index(:accounts, [:username])
  end
end
