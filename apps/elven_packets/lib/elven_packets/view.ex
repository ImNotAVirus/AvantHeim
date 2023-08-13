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
    Map.get(args, key) || raise ArgumentError, "args must define #{key}"
  end
end