defmodule EntityCase do
  use ExUnit.CaseTemplate

  alias CachingService.Entity.Character

  setup do
    # This will run before each test that uses this case
    :ok
  end

  using do
    quote do
      import EntityCase,
        only: [
          character_mock: 0,
          character_mock: 1,
          character_attrs_mock: 0,
          character_attrs_mock: 1
        ]
    end
  end

  ## Mocks

  def character_mock(attrs \\ %{}) do
    attrs |> character_attrs_mock |> Character.new()
  end

  def character_attrs_mock(attrs \\ %{}) do
    Map.merge(
      %{
        id: random_integer(),
        account_id: random_integer(),
        name: random_string(),
        gender: :male,
        class: :adventurer,
        hair_color: :dark_purple,
        hair_style: :hair_style_b,
        faction: :demon,
        map_vnum: 2,
        map_x: 3,
        map_y: 4,
        level: 5,
        job_level: 6,
        hero_level: 7,
        level_xp: 8,
        job_level_xp: 9,
        hero_level_xp: 10,
        gold: 11,
        bank_gold: 12,
        socket: :this_is_a_socket
      },
      attrs
    )
  end

  ## Helpers

  defp random_integer() do
    System.unique_integer([:positive])
  end

  defp random_string() do
    for _ <- 1..10, into: "", do: <<Enum.random('0123456789abcdef')>>
  end
end
