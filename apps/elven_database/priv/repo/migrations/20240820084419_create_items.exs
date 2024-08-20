defmodule ElvenDatabase.Repo.Migrations.CreateItems do
  use Ecto.Migration
  
  require ElvenDatabase.EctoEnumHelpers, as: EctoEnumHelpers
  require ElvenData.Enums.ItemEnums, as: ItemEnums

  def change() do
    execute(
      EctoEnumHelpers.create_query(ItemEnums, :inventory_type),
      EctoEnumHelpers.drop_query(:inventory_type)
    )

    create table(:items) do
      add :owner_id, references(:characters), null: false
      add :inventory_type, :inventory_type_enum, null: false
      add :slot, :int2, null: false
      add :vnum, :int2, null: false
      add :quantity, :int2, null: false

      timestamps()
    end
  end
end
