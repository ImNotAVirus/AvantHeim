defmodule ElvenViews.MapEnumsTest do
  use EnumCase, async: true

  require ElvenData.Enums.MapEnums
  alias ElvenData.Enums.MapEnums

  test_enum MapEnums, :spawn_effect_type
  test_enum MapEnums, :portal_direction_type
end
