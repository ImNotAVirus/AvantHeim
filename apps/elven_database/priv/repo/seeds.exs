# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     ElvenDatabase.Repo.insert!(%ElvenDatabase.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias ElvenDatabase.Players.{
  Account,
  Accounts,
  Characters,
  Items
}

## Accounts

%Account{id: admin_id} =
  Accounts.create!(%{
    username: "admin",
    password: "admin",
    authority: :administrator
  })

%Account{id: user_id} =
  Accounts.create!(%{
    username: "user",
    password: "user"
  })

## Characters

admin_char = Characters.create!(%{
  account_id: admin_id,
  slot: 1,
  name: "DarkyZ",
  gender: :female,
  hair_style: :hair_style_a,
  hair_color: :dark_purple,
  class: :martial_artist,
  faction: :demon,
  map_id: 1,
  map_x: :rand.uniform(3) + 77,
  map_y: :rand.uniform(4) + 113,
  gold: 1_000_000_000,
  bank_gold: 5_000_000,
  biography: "Hi guys! I'm DarkyZ",
  level: 96,
  job_level: 80,
  hero_level: 25,
  level_xp: 3_000,
  job_level_xp: 4_500,
  hero_level_xp: 1_000,
  reputation: 5_000_000,
  dignity: 100,
  sp_points: 10_000,
  sp_additional_points: 500_000,
  compliment: 500
})

user_char = Characters.create!(%{
  account_id: user_id,
  slot: 0,
  name: "ExampleUser",
  gender: :female,
  hair_style: :hair_style_a,
  hair_color: :dark_purple,
  class: :archer,
  faction: :angel,
  map_id: 1,
  map_x: :rand.uniform(3) + 77,
  map_y: :rand.uniform(4) + 113,
  gold: 1_000_000,
  level: 20,
  job_level: 20,
  reputation: 1_000,
  dignity: 100,
  compliment: 50
})

## Base items

base_items = [
  %{
    inventory_type: :equipped,
    slot: :main_weapon,
    vnum: 1,
    quantity: 1,
  },
  %{
    inventory_type: :equipped,
    slot: :armor,
    vnum: 12,
    quantity: 1,
  },
  %{
    inventory_type: :equipped,
    slot: :secondary_weapon,
    vnum: 8,
    quantity: 1,
  },
  %{
    inventory_type: :etc,
    slot: 0,
    vnum: 2024,
    quantity: 10,
  },
  %{
    inventory_type: :etc,
    slot: 1,
    vnum: 2081,
    quantity: 1,
  }
]

for item <- base_items, character <- [admin_char, user_char] do
  item
  |> Map.put(:owner, character)
  |> Items.create!()
end
