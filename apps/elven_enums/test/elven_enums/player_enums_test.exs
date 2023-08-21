defmodule ElvenViews.PlayerEnumsTest do
  use EnumCase, async: true

  require ElvenEnums.PlayerEnums
  alias ElvenEnums.PlayerEnums

  ## Account enums
  test_enum PlayerEnums, :language
  test_enum PlayerEnums, :authority

  ## Character enums
  test_enum PlayerEnums, :gender
  test_enum PlayerEnums, :faction
  test_enum PlayerEnums, :miniland_state
  test_enum PlayerEnums, :character_class
  test_enum PlayerEnums, :hair_style
  test_enum PlayerEnums, :hair_color
  test_enum PlayerEnums, :family_rank
end
