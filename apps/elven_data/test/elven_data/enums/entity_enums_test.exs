defmodule ElvenViews.EntityEnumsTest do
  use EnumCase, async: true

  require ElvenData.Enums.EntityEnums
  alias ElvenData.Enums.EntityEnums

  test_enum EntityEnums, :entity_type
  test_enum EntityEnums, :element_type
  test_enum EntityEnums, :direction_type
  test_enum EntityEnums, :fairy_move_type
  test_enum EntityEnums, :fairy_morph_type
end
