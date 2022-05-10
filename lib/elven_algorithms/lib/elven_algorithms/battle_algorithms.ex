defmodule ElvenAlgorithms.BattleAlgorithms do
  @moduledoc """
  TODO: Documentation
  """

  import ElvenAlgorithms

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

  ## MP Max Algorithm

  @mp_constant %{
    adventurer: 0,
    swordman: 0,
    archer: 1,
    magician: 8,
    martial_artist: 2
  }

  for level <- 1..@max_level, {class, multiplier} <- @mp_constant do
    mpx = level + floor((level - 1) * multiplier / 10)

    mp =
      floor(9.25 * mpx + 50.75) +
        (mpx - 2) / 4 * 2 * (modulus(mpx - 2, 4) + 1 + (mpx - 6) / 4 * 2)

    mp_trunc = trunc(mp)

    def mp_max(unquote(class), unquote(level)) do
      unquote(mp_trunc)
    end
  end
end
