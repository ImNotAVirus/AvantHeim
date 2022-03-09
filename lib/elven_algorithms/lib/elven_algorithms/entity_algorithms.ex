defmodule ElvenAlgorithms.EntityAlgorithms do
  @moduledoc """
  TODO: Documentation
  """

  ## Speed Algorithm

  @speed_constant %{
    adventurer: 11,
    swordman: 11,
    archer: 12,
    magician: 10,
    martial_artist: 11
  }

  for {class, speed} <- @speed_constant do
    def speed(unquote(class)) do
      unquote(speed)
    end
  end
end
