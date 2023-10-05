defmodule ElvenAlgorithms.Pathfinding do
  @moduledoc """
  TODO: Documentation for ElvenAlgorithms.Pathfinding
  """

  use Rustler, otp_app: :elven_algorithms, crate: "pathfinding"

  # ElvenAlgorithms.Pathfinding.astar(<<0,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0>>,5,5,{0,0},{4,4})
  @type coord :: {non_neg_integer(), non_neg_integer()}
  @spec astar(binary(), pos_integer(), pos_integer(), coord(), coord()) ::
          {:ok, [coord()]} | {:error, atom()}
  def astar(_map, _width, _height, _start, _end), do: err()

  # Helpers

  defp err(), do: :erlang.nif_error(:nif_not_loaded)
end
