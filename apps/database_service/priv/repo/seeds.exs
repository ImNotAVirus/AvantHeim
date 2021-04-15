# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     DatabaseService.Repo.insert!(%DatabaseService.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias DatabaseService.Players.Accounts

Accounts.create!(%{
  username: "admin",
  password: "admin",
  authority: :administrator
})

Accounts.create!(%{
  username: "user",
  password: "user"
})
