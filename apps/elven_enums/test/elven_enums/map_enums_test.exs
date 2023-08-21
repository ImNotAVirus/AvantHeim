defmodule ElvenViews.MapEnumsTest do
  use EnumCase, async: true

  require ElvenEnums.MapEnums
  alias ElvenEnums.MapEnums

  test_enum MapEnums, :spawn_effect_type
end
