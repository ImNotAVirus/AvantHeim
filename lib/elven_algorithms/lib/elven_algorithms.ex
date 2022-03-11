defmodule ElvenAlgorithms do
  @moduledoc """
  Documentation for `ElvenAlgorithms`.
  """

  def sign(number) do
    case number do
      x when x < 0 -> -1
      0 -> 0
      _ -> 1
    end
  end

  def modulus(dividend, divisor) do
    (abs(dividend) - abs(divisor) * floor(abs(dividend) / abs(divisor))) * sign(dividend)
  end
end
