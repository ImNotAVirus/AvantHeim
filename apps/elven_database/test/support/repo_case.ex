defmodule ElvenDatabase.RepoCase do
  use ExUnit.CaseTemplate

  require ElvenData.Enums.PlayerEnums

  alias ElvenData.Enums.PlayerEnums

  ## Case

  using do
    quote do
      import Ecto
      import Ecto.Query
      import unquote(__MODULE__), only: :functions

      alias ElvenDatabase.Repo
    end
  end

  setup tags do
    pid = Ecto.Adapters.SQL.Sandbox.start_owner!(ElvenDatabase.Repo, shared: not tags[:async])
    on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)
    :ok
  end

  ## Public API

  def random_string() do
    :crypto.strong_rand_bytes(5) |> Base.encode16(case: :lower)
  end

  def changeset_error(%Ecto.Changeset{errors: errors}) do
    errors
    |> Enum.map(fn {field, {error, _}} -> "#{field} #{error}" end)
    |> Enum.join(" - ")
  end

  def account_attrs() do
    %{
      username: random_string(),
      password: random_string(),
      authority: Enum.random(PlayerEnums.authority(:__keys__))
    }
  end

  def character_attrs(attrs \\ %{}) do
    base_attrs =
      %{
        slot: 0,
        name: random_string(),
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
        biography: nil,
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
      }

    base_attrs = Map.merge(base_attrs, attrs)

    case attrs do
      %{account: account} -> Map.put(base_attrs, :account, account)
      %{account_id: account_id} -> Map.put(base_attrs, :account_id, account_id)
      _ -> base_attrs
    end
  end
end
