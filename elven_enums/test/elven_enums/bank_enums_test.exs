defmodule ElvenViews.BankEnumsTest do
  use EnumCase, async: true

  require ElvenEnums.BankEnums
  alias ElvenEnums.BankEnums

  test_enum BankEnums, :action_type
  test_enum BankEnums, :text_color
end
