defmodule FakeData do
  @moduledoc false

  def equipments(character_id: _) do
    [nil] |> Stream.cycle() |> Enum.take(10)
  end

  def hp(character_id: _), do: 10_000
  def hp_max(character_id: _), do: 10_000
  def mp(character_id: _), do: 5_000
  def mp_max(character_id: _), do: 5_000

  def authority(character_id: 1), do: :game_master
  def authority(character_id: _), do: :player
  def name_color_id(character_id: 1), do: 2
  def name_color_id(character_id: _), do: 0
  def morph(character_id: 1), do: 0
  def morph(character_id: _), do: 24
  def morph_upgrade(character_id: _), do: 15
  def morph_design(character_id: _), do: 12
  def is_invisible(character_id: _), do: false
  def is_arena_winner(character_id: 1), do: true
  def is_arena_winner(character_id: _), do: true
  def size(character_id: _), do: 10
  def item_morph(character_id: _), do: 0

  def direction(character_id: _), do: :south
  def map_music(character_id: _), do: 1
  def can_attack(character_id: _), do: true
  def can_move(character_id: _), do: true

  def group_id(character_id: _), do: -1

  def family_id(character_id: 1), do: 1337
  def family_id(character_id: _), do: -1
  def family_rank(character_id: 1), do: :head
  def family_rank(character_id: _), do: :member
  def family_name(character_id: 1), do: "Alchemists"
  def family_name(character_id: _), do: nil
  def family_level(character_id: 1), do: 20
  def family_level(character_id: _), do: 0
  def family_icons(character_id: 1), do: [true, true, true]
  def family_icons(character_id: _), do: [false, false, false]

  def cp(character_id: _), do: 100
  def hero_level(character_id: _), do: 0
  def hero_level_xp(character_id: _), do: 0
  def level_xp_max(character_id: _), do: 10_000
  def job_level_xp_max(character_id: _), do: 10_000
  def hero_level_xp_max(character_id: _), do: 0

  def dignity(character_id: _), do: 100
  def dignity_icon_id(character_id: _), do: 1
  def reputation(character_id: 1), do: 10_000_000
  def reputation(character_id: _), do: 1_000
  def reputation_icon_id(character_id: 1), do: 32
  def reputation_icon_id(character_id: _), do: 1
  def compliment(character_id: 1), do: 500
  def compliment(character_id: _), do: 500
  def title_id(character_id: 1), do: 10
  def title_id(character_id: _), do: 0

  def fairy_morph(character_id: _), do: 13
  def fairy_move_type_id(character_id: _), do: 2
  def fairy_element(character_id: _), do: :darkness
end
