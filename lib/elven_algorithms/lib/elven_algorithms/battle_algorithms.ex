defmodule ElvenAlgorithms.BattleAlgorithms do
  @moduledoc """
  TODO: Documentation
  """

  @max_level 99

  ## HP Max Algorithm

  @hp_constant %{
    adventurer: 0,
    swordman: 8,
    archer: 3,
    magician: 0,
    martial_artist: 5
  }

  for level <- 1..@max_level, {class, multiplier} <- @hp_constant do
    hpx = level + floor((level - 1) * multiplier / 10)
    hp = trunc(0.5 * hpx ** 2 + 15.5 * hpx + 205)

    def hp_max(unquote(class), unquote(level)) do
      unquote(hp)
    end
  end
end
