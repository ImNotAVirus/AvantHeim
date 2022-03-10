defmodule ElvenViews do
  @moduledoc """
  Documentation for `ElvenViews`.

  TODO: Add ElvenViews/1
  """

  @callback render(atom, map) :: struct

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      import ElvenViews, only: [optional_param: 2, optional_param: 3, required_param: 2]

      def render(key), do: render(key, %{})
    end
  end

  @spec optional_param(map, atom, any) :: any
  def optional_param(args, key, default \\ nil) do
    args[key] || default
  end

  @spec required_param(map, atom) :: any
  def required_param(args, key) do
    args[key] || raise ArgumentError, "args must define #{key}"
  end
end
