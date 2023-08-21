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
  def morph(character_id: 1), do: :default
  def morph(character_id: _), do: :demon_hunter
  def morph_upgrade(character_id: _), do: 15
  def wings_design(character_id: _), do: :tree
  def is_invisible(character_id: _), do: false
  def is_arena_winner(character_id: 1), do: true
  def is_arena_winner(character_id: _), do: false
  def size(character_id: _), do: 10
  def item_morph(character_id: _), do: 0

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
  def level_xp_max(character_id: _), do: 10_000
  def job_level_xp_max(character_id: _), do: 10_000
  def hero_level_xp_max(character_id: _), do: 0

  def dignity(character_id: _), do: 100
  def dignity_icon(character_id: _), do: :basic
  def reputation(character_id: 1), do: 10_000_000
  def reputation(character_id: _), do: 1_000
  def reputation_icon(character_id: 1), do: :legendary_hero
  def reputation_icon(character_id: _), do: :green_beginner
  def compliment(character_id: 1), do: 500
  def compliment(character_id: _), do: 500
  def title_id(character_id: 1), do: 10
  def title_id(character_id: _), do: 0

  def fairy_morph(character_id: _), do: 13
  def fairy_move_type_id(character_id: _), do: 2
  def fairy_element(character_id: _), do: :darkness
end
