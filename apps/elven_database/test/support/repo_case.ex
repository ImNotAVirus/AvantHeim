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

  @doc """
  Return a random string.
  """
  @spec random_string() :: String.t()
  def random_string() do
    :crypto.strong_rand_bytes(5) |> Base.encode16(case: :lower)
  end

  @doc """
  Transform an `Ecto.Changeset.t()` into a string.
  """
  @spec changeset_error(Ecto.Changeset.t()) :: String.t()
  def changeset_error(%Ecto.Changeset{errors: errors}) do
    errors
    |> Enum.map(fn {field, {error, opts}} ->
      full_error =
        error
        |> then(&Regex.scan(~r/%\{(\w+)\}/, &1, capture: :all_but_first))
        |> List.flatten()
        |> Enum.uniq()
        |> Enum.reduce(error, &String.replace(&2, "%{#{&1}}", "#{opts[String.to_atom(&1)]}"))

      "#{field} #{full_error}"
    end)
    |> Enum.join(" - ")
  end

  @doc """
  Return a map containing random attributes for an account creation.
  """
  @spec account_attrs() :: map()
  def account_attrs() do
    %{
      username: random_string(),
      password: random_string(),
      authority: Enum.random(PlayerEnums.authority(:__keys__))
    }
  end

  @doc """
  Return a map containing random attributes for a character creation.
  """
  @spec character_attrs(map()) :: map()
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

  @doc """
  Return a map containing random attributes for an item creation.
  """
  @spec item_attrs(map()) :: map()
  def item_attrs(attrs \\ %{}) do
    base_attrs =
      %{
        inventory_type: :etc,
        slot: 0,
        vnum: Enum.random(1..9999),
        quantity: Enum.random(1..100)
      }

    base_attrs = Map.merge(base_attrs, attrs)

    case attrs do
      %{owner: owner} -> Map.put(base_attrs, :owner, owner)
      %{owner_id: owner_id} -> Map.put(base_attrs, :owner_id, owner_id)
      _ -> base_attrs
    end
  end
end
