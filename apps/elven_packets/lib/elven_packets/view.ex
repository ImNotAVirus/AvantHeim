defmodule ElvenPackets.View do
  @moduledoc """
  TODO: ElvenPackets.View
  """

  @spec optional_param(map(), atom(), any()) :: any
  def optional_param(args, key, default \\ nil) do
    Map.get(args, key, default)
  end

  @spec required_param(map(), atom()) :: any()
  def required_param(args, key) do
    case Map.get(args, key) do
      nil -> raise ArgumentError, "args must define #{key}"
      value -> value
    end
  end
end
