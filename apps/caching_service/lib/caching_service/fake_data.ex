defmodule FakeData.Character do
  @keys [
    :id,
    :name,
    :gender,
    :class,
    :hair_color,
    :hair_style,
    :map_id,
    :map_x,
    :map_y,
    :level,
    :job_level,
    :hero_level,
    :level_xp,
    :job_level_xp,
    :hero_level_xp
  ]

  defstruct @keys
end

defmodule FakeData do
  @moduledoc false

  alias FakeData.Character

  def character(id: 1) do
    %Character{
      id: 1,
      name: "DarkyZ",
      gender: :female,
      class: :martial_artist,
      hair_color: :dark_purple,
      hair_style: :hair_style_a,
      #
      map_id: 1,
      map_x: :rand.uniform(3) + 77,
      map_y: :rand.uniform(4) + 113,
      #
      level: 96,
      job_level: 80,
      hero_level: 25,
      level_xp: 0,
      job_level_xp: 0,
      hero_level_xp: 0
    }
  end

  def equipments(character_id: _) do
    [nil] |> Stream.cycle() |> Enum.take(10)
  end
end
