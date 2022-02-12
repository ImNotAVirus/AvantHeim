defmodule ElvenViews do
  @moduledoc """
  Documentation for `ElvenViews`.

  TODO: Views behaviour
  """

  @callback render(atom, map) :: struct

  @spec optional_param(map, atom) :: any
  def optional_param(args, key) do
    args[key]
  end

  @spec required_param(map, atom) :: any
  def required_param(args, key) do
    args[key] || raise ArgumentError, "args must define #{key}"
  end
end
