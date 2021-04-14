defmodule DatabaseService.Repo.Migrations.CreateAccounts do
  use Ecto.Migration
  
  import DatabaseService.EctoAuthority, only: [authority: 1]
  
  alias DatabaseService.EctoAuthority

  def change do
    execute(
      "CREATE TYPE language_enum AS ENUM ('en', 'fr')",
      "DROP TYPE language_enum"
    )

    create table(:accounts) do
      add :username, :string, null: false
      add :password, :string, null: false, size: 128
      add :authority, EctoAuthority.type(), null: false, default: authority(:player)
      add :language, :language_enum, null: false, default: "en"

      timestamps()
    end

    create unique_index(:accounts, [:username])
  end
end
