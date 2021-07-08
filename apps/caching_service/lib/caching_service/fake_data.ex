defmodule FakeData.Character do
  @enforce_keys [
    :id,
    :name,
    :gender,
    :class,
    :hair_color,
    :hair_style,
    :faction,
    :map_vnum,
    :map_x,
    :map_y,
    :level,
    :job_level,
    :hero_level,
    :level_xp,
    :job_level_xp,
    :hero_level_xp
  ]

  defstruct @enforce_keys
end

defmodule FakeData.Posiion do
  @enforce_keys [:map_id, :map_vnum, :map_x, :map_y, :is_instance]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          map_id: pos_integer | reference,
          map_vnum: pos_integer,
          map_x: non_neg_integer,
          map_y: non_neg_integer,
          map_y: boolean
        }
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
      faction: :demon,
      #
      map_vnum: 1,
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

  def get_position(character_id: _) do
    %FakeData.Posiion{
      map_id: 1,
      map_vnum: 1,
      map_x: :rand.uniform(3) + 77,
      map_y: :rand.uniform(4) + 113,
      is_instance: true
    }
  end

  def equipments(character_id: _) do
    [nil] |> Stream.cycle() |> Enum.take(10)
  end

  def hp(character_id: _), do: 10_000
  def hp_max(character_id: _), do: 10_000
  def mp(character_id: _), do: 5_000
  def mp_max(character_id: _), do: 5_000

  def name_color_id(character_id: 1), do: 2
  def morph(character_id: _), do: 0
  def morph_upgrade(character_id: _), do: 0
  def morph_design(character_id: _), do: 0
  def invisible(character_id: _), do: false
  def arena_winner(character_id: 1), do: true
  def size(character_id: _), do: 10
  def item_morph(character_id: _), do: 0

  def speed(character_id: _), do: 20
  def direction(character_id: _), do: :south
  def map_music(character_id: _), do: 1
  def no_attack(character_id: _), do: false
  def no_move(character_id: _), do: false

  def group_id(character_id: _), do: -1

  def family_id(character_id: 1), do: 1337
  def family_rank(character_id: 1), do: :head
  def family_name(character_id: 1), do: "Alchemists"
  def family_level(character_id: 1), do: 20

  def cp(character_id: _), do: 100
  def level_xp_max(character_id: _), do: 10_000
  def job_level_xp_max(character_id: _), do: 10_000
  def hero_level_xp_max(character_id: _), do: 10_000

  def dignity(character_id: _), do: 100
  def dignity_icon_id(character_id: _), do: 1
  def reputation(character_id: 1), do: 10_000_000
  def reputation_icon_id(character_id: 1), do: 32
  def compliment(character_id: 1), do: 500
end
